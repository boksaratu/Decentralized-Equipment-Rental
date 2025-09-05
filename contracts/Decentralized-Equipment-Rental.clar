;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-LISTED (err u101))
(define-constant ERR-NOT-LISTED (err u102))
(define-constant ERR-ALREADY-RENTED (err u103))
(define-constant ERR-NOT-AVAILABLE (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-RENTAL-NOT-EXPIRED (err u106))
(define-constant ERR-ALREADY-REVIEWED (err u107))
(define-constant ERR-INVALID-RATING (err u108))
(define-constant ERR-NOT-RENTED-BY-USER (err u109))
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u110))
(define-constant ERR-DEPOSIT-ALREADY-CLAIMED (err u111))
(define-constant ERR-NO-DEPOSIT-FOUND (err u112))
(define-constant ERR-STILL-RENTED (err u113))
(define-constant ERR-MAINTENANCE-ACTIVE (err u114))
(define-constant ERR-MAINTENANCE-NOT-FOUND (err u115))
(define-constant ERR-MAINTENANCE-ALREADY-COMPLETED (err u116))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var late-fee-rate uint u10)
(define-data-var deposit-percentage uint u50)

;; Maps
(define-map equipment-listings
  { equipment-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    daily-rate: uint,
    available: bool,
    current-renter: (optional principal),
    rental-start: (optional uint),
    rental-end: (optional uint)
  }
)

(define-map user-rentals
  { user: principal }
  { active-rentals: (list 10 uint) }
)

(define-map equipment-reviews
  { equipment-id: uint, reviewer: principal }
  {
    rating: uint,
    review-text: (string-ascii 200),
    review-date: uint
  }
)

(define-map equipment-ratings
  { equipment-id: uint }
  {
    total-rating: uint,
    review-count: uint,
    average-rating: uint
  }
)

(define-map owner-reputation
  { owner: principal }
  {
    total-rating: uint,
    review-count: uint,
    average-rating: uint
  }
)

(define-map rental-history
  { equipment-id: uint, renter: principal }
  { has-rented: bool }
)

(define-map security-deposits
  { equipment-id: uint, renter: principal }
  {
    amount: uint,
    deposited-at: uint,
    claimed: bool,
    refunded: bool
  }
)

(define-map maintenance-records
  { equipment-id: uint, maintenance-id: uint }
  {
    owner: principal,
    start-block: uint,
    end-block: uint,
    maintenance-type: (string-ascii 100),
    completed: bool,
    cost: uint
  }
)

(define-map equipment-maintenance-counter
  { equipment-id: uint }
  { next-maintenance-id: uint }
)

;; Public functions
(define-public (list-equipment (equipment-id uint) (name (string-ascii 50)) (daily-rate uint))
  (let ((listing (map-get? equipment-listings { equipment-id: equipment-id })))
    (asserts! (is-none listing) ERR-ALREADY-LISTED)
    (ok (map-set equipment-listings
      { equipment-id: equipment-id }
      {
        owner: tx-sender,
        name: name,
        daily-rate: daily-rate,
        available: true,
        current-renter: none,
        rental-start: none,
        rental-end: none
      }))))

(define-public (rent-equipment (equipment-id uint) (days uint))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (total-cost (* (get daily-rate listing) days))
    (deposit-amount (/ (* (get daily-rate listing) days (var-get deposit-percentage)) u100))
    (total-payment (+ total-cost deposit-amount))
    (current-height stacks-block-height)
  )
    (asserts! (get available listing) ERR-NOT-AVAILABLE)
    (asserts! (not (is-equipment-under-maintenance equipment-id)) ERR-MAINTENANCE-ACTIVE)
    (try! (stx-transfer? total-payment tx-sender (get owner listing)))
    
    (map-set security-deposits
      { equipment-id: equipment-id, renter: tx-sender }
      {
        amount: deposit-amount,
        deposited-at: current-height,
        claimed: false,
        refunded: false
      })
    
    (map-set equipment-listings
      { equipment-id: equipment-id }
      (merge listing {
        available: false,
        current-renter: (some tx-sender),
        rental-start: (some current-height),
        rental-end: (some (+ current-height (* days u144)))
      }))
    
    (let ((user-rental (default-to { active-rentals: (list) } 
          (map-get? user-rentals { user: tx-sender }))))
      (map-set user-rentals
        { user: tx-sender }
        { active-rentals: (unwrap-panic (as-max-len? 
          (append (get active-rentals user-rental) equipment-id) u10)) }))
    (ok true)))

(define-public (return-equipment (equipment-id uint))
  (let ((listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED)))
    (asserts! (is-eq (some tx-sender) (get current-renter listing)) ERR-NOT-AUTHORIZED)
    
    (map-set rental-history
      { equipment-id: equipment-id, renter: tx-sender }
      { has-rented: true })
    
    (map-set equipment-listings
      { equipment-id: equipment-id }
      (merge listing {
        available: true,
        current-renter: none,
        rental-start: none,
        rental-end: none
      }))
    (ok true)))

(define-public (force-return-expired (equipment-id uint))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (rental-end (unwrap! (get rental-end listing) ERR-NOT-LISTED))
    (current-renter (unwrap! (get current-renter listing) ERR-NOT-LISTED))
    (current-height stacks-block-height)
    (days-overdue (/ (- current-height rental-end) u144))
    (late-fee (* days-overdue (get daily-rate listing) (var-get late-fee-rate)))
    (total-late-fee (/ late-fee u100))
  )
    (asserts! (> current-height rental-end) ERR-RENTAL-NOT-EXPIRED)
    
    (if (> total-late-fee u0)
      (try! (stx-transfer? total-late-fee current-renter (get owner listing)))
      true)
    
    (map-set equipment-listings
      { equipment-id: equipment-id }
      (merge listing {
        available: true,
        current-renter: none,
        rental-start: none,
        rental-end: none
      }))
    (ok true)))

(define-public (set-late-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set late-fee-rate new-rate)
    (ok true)))

(define-public (set-deposit-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set deposit-percentage new-percentage)
    (ok true)))

(define-public (refund-deposit (equipment-id uint) (renter principal))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (deposit (unwrap! (map-get? security-deposits { equipment-id: equipment-id, renter: renter }) ERR-NO-DEPOSIT-FOUND))
    (deposit-amount (get amount deposit))
  )
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get refunded deposit)) ERR-DEPOSIT-ALREADY-CLAIMED)
    (asserts! (not (get claimed deposit)) ERR-DEPOSIT-ALREADY-CLAIMED)
    (asserts! (get available listing) ERR-STILL-RENTED)
    
    (try! (stx-transfer? deposit-amount (get owner listing) renter))
    
    (map-set security-deposits
      { equipment-id: equipment-id, renter: renter }
      (merge deposit { refunded: true }))
    (ok true)))

(define-public (claim-deposit (equipment-id uint) (renter principal))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (deposit (unwrap! (map-get? security-deposits { equipment-id: equipment-id, renter: renter }) ERR-NO-DEPOSIT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get claimed deposit)) ERR-DEPOSIT-ALREADY-CLAIMED)
    (asserts! (not (get refunded deposit)) ERR-DEPOSIT-ALREADY-CLAIMED)
    
    (map-set security-deposits
      { equipment-id: equipment-id, renter: renter }
      (merge deposit { claimed: true }))
    (ok true)))

(define-public (submit-review (equipment-id uint) (rating uint) (review-text (string-ascii 200)))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (rental-record (map-get? rental-history { equipment-id: equipment-id, renter: tx-sender }))
    (existing-review (map-get? equipment-reviews { equipment-id: equipment-id, reviewer: tx-sender }))
    (current-height stacks-block-height)
    (equipment-owner (get owner listing))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-some rental-record) ERR-NOT-RENTED-BY-USER)
    (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
    
    (map-set equipment-reviews
      { equipment-id: equipment-id, reviewer: tx-sender }
      {
        rating: rating,
        review-text: review-text,
        review-date: current-height
      })
    
    (let (
      (current-equipment-rating (default-to { total-rating: u0, review-count: u0, average-rating: u0 }
        (map-get? equipment-ratings { equipment-id: equipment-id })))
      (new-total-rating (+ (get total-rating current-equipment-rating) rating))
      (new-review-count (+ (get review-count current-equipment-rating) u1))
      (new-average-rating (/ new-total-rating new-review-count))
    )
      (map-set equipment-ratings
        { equipment-id: equipment-id }
        {
          total-rating: new-total-rating,
          review-count: new-review-count,
          average-rating: new-average-rating
        }))
    
    (let (
      (current-owner-reputation (default-to { total-rating: u0, review-count: u0, average-rating: u0 }
        (map-get? owner-reputation { owner: equipment-owner })))
      (owner-new-total (+ (get total-rating current-owner-reputation) rating))
      (owner-new-count (+ (get review-count current-owner-reputation) u1))
      (owner-new-average (/ owner-new-total owner-new-count))
    )
      (map-set owner-reputation
        { owner: equipment-owner }
        {
          total-rating: owner-new-total,
          review-count: owner-new-count,
          average-rating: owner-new-average
        }))
    (ok true)))

(define-public (schedule-maintenance (equipment-id uint) (start-block uint) (end-block uint) (maintenance-type (string-ascii 100)) (cost uint))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (counter (default-to { next-maintenance-id: u1 } 
      (map-get? equipment-maintenance-counter { equipment-id: equipment-id })))
    (maintenance-id (get next-maintenance-id counter))
    (current-height stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    (asserts! (> start-block current-height) ERR-NOT-AUTHORIZED)
    (asserts! (> end-block start-block) ERR-NOT-AUTHORIZED)
    (asserts! (get available listing) ERR-NOT-AVAILABLE)
    
    (map-set maintenance-records
      { equipment-id: equipment-id, maintenance-id: maintenance-id }
      {
        owner: tx-sender,
        start-block: start-block,
        end-block: end-block,
        maintenance-type: maintenance-type,
        completed: false,
        cost: cost
      })
    
    (map-set equipment-maintenance-counter
      { equipment-id: equipment-id }
      { next-maintenance-id: (+ maintenance-id u1) })
    
    (ok maintenance-id)))

(define-public (complete-maintenance (equipment-id uint) (maintenance-id uint))
  (let (
    (listing (unwrap! (map-get? equipment-listings { equipment-id: equipment-id }) ERR-NOT-LISTED))
    (maintenance (unwrap! (map-get? maintenance-records { equipment-id: equipment-id, maintenance-id: maintenance-id }) ERR-MAINTENANCE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner listing)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get completed maintenance)) ERR-MAINTENANCE-ALREADY-COMPLETED)
    
    (map-set maintenance-records
      { equipment-id: equipment-id, maintenance-id: maintenance-id }
      (merge maintenance { completed: true }))
    
    (ok true)))

;; Read-only functions
(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment-listings { equipment-id: equipment-id }))

(define-read-only (get-user-rentals (user principal))
  (map-get? user-rentals { user: user }))

(define-read-only (get-equipment-rating (equipment-id uint))
  (map-get? equipment-ratings { equipment-id: equipment-id }))

(define-read-only (get-owner-reputation (owner principal))
  (map-get? owner-reputation { owner: owner }))

(define-read-only (get-review (equipment-id uint) (reviewer principal))
  (map-get? equipment-reviews { equipment-id: equipment-id, reviewer: reviewer }))

(define-read-only (is-rental-expired (equipment-id uint))
  (let ((listing (map-get? equipment-listings { equipment-id: equipment-id })))
    (match listing
      equipment-data
        (match (get rental-end equipment-data)
          rental-end-block
            (> stacks-block-height rental-end-block)
          false)
      false)))

(define-read-only (calculate-late-fee (equipment-id uint))
  (let (
    (listing (map-get? equipment-listings { equipment-id: equipment-id }))
    (current-height stacks-block-height)
  )
    (match listing
      equipment-data
        (match (get rental-end equipment-data)
          rental-end-block
            (if (> current-height rental-end-block)
              (let (
                (days-overdue (/ (- current-height rental-end-block) u144))
                (daily-rate (get daily-rate equipment-data))
                (late-fee-percentage (var-get late-fee-rate))
              )
                (/ (* days-overdue daily-rate late-fee-percentage) u100))
              u0)
          u0)
      u0)))

(define-read-only (can-review (equipment-id uint) (reviewer principal))
  (let (
    (rental-record (map-get? rental-history { equipment-id: equipment-id, renter: reviewer }))
    (existing-review (map-get? equipment-reviews { equipment-id: equipment-id, reviewer: reviewer }))
  )
    (and (is-some rental-record) (is-none existing-review))))

(define-read-only (get-security-deposit (equipment-id uint) (renter principal))
  (map-get? security-deposits { equipment-id: equipment-id, renter: renter }))

(define-read-only (calculate-deposit-amount (equipment-id uint) (days uint))
  (let ((listing (map-get? equipment-listings { equipment-id: equipment-id })))
    (match listing
      equipment-data
        (/ (* (get daily-rate equipment-data) days (var-get deposit-percentage)) u100)
      u0)))

(define-read-only (get-deposit-percentage)
  (var-get deposit-percentage))

(define-read-only (get-maintenance-record (equipment-id uint) (maintenance-id uint))
  (map-get? maintenance-records { equipment-id: equipment-id, maintenance-id: maintenance-id }))

(define-read-only (is-equipment-under-maintenance (equipment-id uint))
  (let (
    (current-height stacks-block-height)
    (counter (map-get? equipment-maintenance-counter { equipment-id: equipment-id }))
  )
    (match counter
      counter-data
        (let ((max-id (get next-maintenance-id counter-data)))
          (get under-maintenance (fold check-maintenance-window (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) 
            { equipment-id: equipment-id, current-height: current-height, max-id: max-id, under-maintenance: false })))
      false)))

(define-private (check-maintenance-window (maintenance-id uint) (state { equipment-id: uint, current-height: uint, max-id: uint, under-maintenance: bool }))
  (if (or (get under-maintenance state) (>= maintenance-id (get max-id state)))
    state
    (let (
      (maintenance (map-get? maintenance-records 
        { equipment-id: (get equipment-id state), maintenance-id: maintenance-id }))
    )
      (match maintenance
        maintenance-data
          (if (and 
                (not (get completed maintenance-data))
                (>= (get current-height state) (get start-block maintenance-data))
                (< (get current-height state) (get end-block maintenance-data)))
            (merge state { under-maintenance: true })
            state)
        state))))
