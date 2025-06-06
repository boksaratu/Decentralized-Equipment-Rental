(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-LISTED (err u101))
(define-constant ERR-NOT-LISTED (err u102))
(define-constant ERR-ALREADY-RENTED (err u103))
(define-constant ERR-NOT-AVAILABLE (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))

(define-data-var contract-owner principal tx-sender)

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
    (current-height stacks-block-height)
  )
    (asserts! (get available listing) ERR-NOT-AVAILABLE)
    (try! (stx-transfer? total-cost tx-sender (get owner listing)))
    
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
    
    (map-set equipment-listings
      { equipment-id: equipment-id }
      (merge listing {
        available: true,
        current-renter: none,
        rental-start: none,
        rental-end: none
      }))
    (ok true)))

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment-listings { equipment-id: equipment-id }))

(define-read-only (get-user-rentals (user principal))
  (map-get? user-rentals { user: user }))
