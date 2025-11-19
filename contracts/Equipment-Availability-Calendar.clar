;; Equipment Availability Calendar Contract
;; Independent feature for managing equipment availability windows and blackout periods

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u201))
(define-constant ERR-AVAILABILITY-CONFLICT (err u202))
(define-constant ERR-INVALID-DATE-RANGE (err u203))
(define-constant ERR-AVAILABILITY-NOT-FOUND (err u204))
(define-constant ERR-BLACKOUT-PERIOD-ACTIVE (err u205))

;; Data variables
(define-data-var contract-owner principal tx-sender)

;; Equipment registry for ownership verification
(define-map equipment-owners
  { equipment-id: uint }
  { owner: principal }
)

;; Equipment Availability Calendar Maps
(define-map equipment-availability-windows
  { equipment-id: uint, window-id: uint }
  {
    owner: principal,
    start-block: uint,
    end-block: uint,
    available: bool,
    notes: (string-ascii 100),
    created-at: uint
  }
)

(define-map equipment-availability-counter
  { equipment-id: uint }
  { next-window-id: uint }
)

(define-map equipment-blackout-periods
  { equipment-id: uint, period-id: uint }
  {
    owner: principal,
    start-block: uint,
    end-block: uint,
    reason: (string-ascii 100),
    created-at: uint
  }
)

(define-map equipment-blackout-counter
  { equipment-id: uint }
  { next-period-id: uint }
)

;; Public functions

(define-public (register-equipment (equipment-id uint))
  (let (
    (validated-id equipment-id)
  )
    (asserts! (is-none (map-get? equipment-owners { equipment-id: validated-id })) ERR-EQUIPMENT-NOT-FOUND)
    (map-set equipment-owners { equipment-id: validated-id } { owner: tx-sender })
    (ok true)))

(define-public (set-availability-window (equipment-id uint) (start-block uint) (end-block uint) (available bool) (notes (string-ascii 100)))
  (let (
    (validated-id equipment-id)
    (validated-notes notes)
    (equipment (unwrap! (map-get? equipment-owners { equipment-id: validated-id }) ERR-EQUIPMENT-NOT-FOUND))
    (counter (default-to { next-window-id: u1 } 
      (map-get? equipment-availability-counter { equipment-id: validated-id })))
    (window-id (get next-window-id counter))
    (current-height stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get owner equipment)) ERR-NOT-AUTHORIZED)
    (asserts! (> end-block start-block) ERR-INVALID-DATE-RANGE)
    (asserts! (>= start-block current-height) ERR-INVALID-DATE-RANGE)
    
    (map-set equipment-availability-windows
      { equipment-id: validated-id, window-id: window-id }
      {
        owner: tx-sender,
        start-block: start-block,
        end-block: end-block,
        available: available,
        notes: validated-notes,
        created-at: current-height
      })
    
    (map-set equipment-availability-counter
      { equipment-id: validated-id }
      { next-window-id: (+ window-id u1) })
    
    (ok window-id)))

;; Read-only functions

(define-read-only (get-equipment-owner (equipment-id uint))
  (let (
    (validated-id equipment-id)
  )
    (map-get? equipment-owners { equipment-id: validated-id })
  )
)

(define-read-only (get-availability-window (equipment-id uint) (window-id uint))
  (let (
    (validated-id equipment-id)
    (validated-wid window-id)
  )
    (map-get? equipment-availability-windows { equipment-id: validated-id, window-id: validated-wid })
  )
)
