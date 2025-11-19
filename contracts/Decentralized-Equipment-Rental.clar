;; Decentralized Equipment Rental Contract
;; Equipment reputation & rating system

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-RATING (err u102))
(define-constant ERR-ALREADY-RATED (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-RATING-NOT-FOUND (err u105))
(define-constant ERR-OWNER-CANNOT-RATE (err u106))

(define-constant SUCCESS-OK (ok true))

(define-data-var contract-owner principal tx-sender)

(define-map equipment-registry
  { equipment-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-ascii 256),
    rental-price: uint,
    created-at: uint,
    total-ratings: uint,
    cumulative-score: uint
  }
)

(define-map equipment-ratings
  { equipment-id: uint, rater: principal }
  {
    rating: uint,
    review: (string-ascii 256),
    rated-at: uint
  }
)

(define-map equipment-reputation
  { equipment-id: uint }
  {
    average-rating: uint,
    total-ratings: uint,
    last-updated: uint
  }
)

(define-public (register-equipment (equipment-id uint) (name (string-ascii 100)) (description (string-ascii 256)) (rental-price uint))
  (let (
    (current-height stacks-block-height)
    (validated-id equipment-id)
    (validated-name name)
    (validated-desc description)
    (validated-price rental-price)
  )
    (asserts! (is-none (map-get? equipment-registry { equipment-id: validated-id })) ERR-EQUIPMENT-NOT-FOUND)
    (asserts! (> validated-price u0) ERR-INVALID-AMOUNT)
    
    (map-set equipment-registry
      { equipment-id: validated-id }
      {
        owner: tx-sender,
        name: validated-name,
        description: validated-desc,
        rental-price: validated-price,
        created-at: current-height,
        total-ratings: u0,
        cumulative-score: u0
      }
    )
    
    (map-set equipment-reputation
      { equipment-id: validated-id }
      {
        average-rating: u0,
        total-ratings: u0,
        last-updated: current-height
      }
    )
    
    SUCCESS-OK
  )
)

(define-public (rate-equipment (equipment-id uint) (rating uint) (review (string-ascii 256)))
  (let (
    (validated-id equipment-id)
    (validated-rating rating)
    (validated-review review)
    (equipment (unwrap! (map-get? equipment-registry { equipment-id: validated-id }) ERR-EQUIPMENT-NOT-FOUND))
    (owner (get owner equipment))
    (current-height stacks-block-height)
    (existing-rating (map-get? equipment-ratings { equipment-id: validated-id, rater: tx-sender }))
    (current-reputation (unwrap! (map-get? equipment-reputation { equipment-id: validated-id }) ERR-RATING-NOT-FOUND))
    (total-ratings (get total-ratings current-reputation))
    (cumulative-score (get cumulative-score equipment))
  )
    (asserts! (and (>= validated-rating u1) (<= validated-rating u5)) ERR-INVALID-RATING)
    (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
    (asserts! (not (is-eq tx-sender owner)) ERR-OWNER-CANNOT-RATE)
    
    (map-set equipment-ratings
      { equipment-id: validated-id, rater: tx-sender }
      {
        rating: validated-rating,
        review: validated-review,
        rated-at: current-height
      }
    )
    
    (let (
      (new-total (+ total-ratings u1))
      (new-cumulative (+ cumulative-score validated-rating))
      (new-average (/ new-cumulative new-total))
    )
      (map-set equipment-registry
        { equipment-id: validated-id }
        (merge equipment {
          total-ratings: new-total,
          cumulative-score: new-cumulative
        })
      )
      
      (map-set equipment-reputation
        { equipment-id: validated-id }
        {
          average-rating: new-average,
          total-ratings: new-total,
          last-updated: current-height
        }
      )
    )
    
    SUCCESS-OK
  )
)

(define-read-only (get-equipment (equipment-id uint))
  (let (
    (validated-id equipment-id)
  )
    (map-get? equipment-registry { equipment-id: validated-id })
  )
)

(define-read-only (get-reputation (equipment-id uint))
  (let (
    (validated-id equipment-id)
  )
    (map-get? equipment-reputation { equipment-id: validated-id })
  )
)

(define-read-only (get-rating (equipment-id uint) (rater principal))
  (let (
    (validated-id equipment-id)
  )
    (map-get? equipment-ratings { equipment-id: validated-id, rater: rater })
  )
)

(define-read-only (can-rate (equipment-id uint) (rater principal))
  (let (
    (equipment (map-get? equipment-registry { equipment-id: equipment-id }))
    (has-existing-rating (is-some (map-get? equipment-ratings { equipment-id: equipment-id, rater: rater })))
  )
    (if (is-none equipment)
      (ok false)
      (if has-existing-rating
        (ok false)
        (if (is-eq rater (get owner (unwrap-panic equipment)))
          (ok false)
          (ok true)
        )
      )
    )
  )
)