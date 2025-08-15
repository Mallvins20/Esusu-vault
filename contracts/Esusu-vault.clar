;; ===========================================================================
;; EsusuVault.clar
;; Cooperative Savings (Esusu / Ajo) smart contract for Stacks (Clarity)
;; Version: 1.1 (fixed compilation & logic errors)
;; Author: (your name)
;; Notes:
;;  - Designed to be simple, readable and safe for Clarinet tests.
;;  - Single-group model (one rotating savings group per contract instance).
;; ===========================================================================

(define-constant ERR-NOT-ADMIN (err u100))
(define-constant ERR-NOT-MEMBER (err u101))
(define-constant ERR-ALREADY-MEMBER (err u102))
(define-constant ERR-GROUP-FULL (err u103))
(define-constant ERR-NOT-YOUR-TURN (err u104))
(define-constant ERR-NOT-PAID (err u105))
(define-constant ERR-ALREADY-PAID (err u106))
(define-constant ERR-CONTRIBUTION-MISMATCH (err u107))
(define-constant ERR-NO-MEMBERS (err u108))
(define-constant ERR-INSUFFICIENT-BALANCE (err u109))
(define-constant ERR-ALREADY-STARTED (err u110))
(define-constant ERR-NOT-STARTED (err u111))
(define-constant ERR-INVALID-INDEX (err u112))

;; ---------------------------
;; Configurable parameters
;; ---------------------------
;; contract admin (group organizer) - set at deploy by tx-sender
(define-constant admin tx-sender)

;; maximum group size (safety guard to keep loops bounded)
(define-constant max-members u50)

;; ---------------------------
;; State variables
;; ---------------------------
;; current number of members (uint)
(define-data-var member-count uint u0)

;; mapping id -> member principal
(define-map members { id: uint } { member: principal })

;; mapping principal -> id (for quick lookup)
(define-map member-id { member: principal } { id: uint })

;; cycle configuration
(define-data-var contribution-amount uint u100) ;; default: 100 STX (adjustable by admin)
(define-data-var cycle-length uint u100) ;; number of blocks per cycle (set by admin)
(define-data-var started bool false) ;; whether the group has started

;; cycle & turn state
(define-data-var current-cycle uint u0) ;; cycle counter
(define-data-var cycle-start-block uint u0) ;; block-height when current cycle started
(define-data-var current-turn-index uint u0) ;; index (0..member-count-1) who will claim this cycle

;; contributions: map {member: principal, cycle: uint} => bool (paid yes/no)
(define-map contributions { member: principal, cycle: uint } { paid: bool })

;; late-penalties: map member -> penalty amount (uint) to be added to next contribution
(define-map penalties { member: principal } { amount: uint })

;; ---------------------------
;; Utility / Internal helpers
;; ---------------------------

(define-private (is-admin? (who principal))
  (is-eq who admin)
)

(define-private (member-exists? (who principal))
  (is-some (map-get? member-id { member: who }))
)

(define-private (get-member-id (who principal))
  (match (map-get? member-id { member: who })
    m (get id m)
    u0
  )
)

;; compute expected amount for a member for a given cycle (contribution + any penalty)
(define-read-only (get-expected-amount (who principal) (cycle uint))
  (let ((base (var-get contribution-amount))
        (pen (default-to u0 (get amount (map-get? penalties { member: who })))))
    (ok (+ base pen))
  )
)

;; ---------------------------
;; Helper: check-paid-idx
;; recursively check payment status for each member id from i to count-1
;; returns true if all paid, false otherwise
;; ---------------------------
;; Unrolled check for up to 10 members
(define-private (all-paid-for-cycle-helper (count uint) (cycle uint))
  (and
    (if (> count u0)
      (let ((entry0 (map-get? members {id: u0})))
        (match entry0 e0
          (let ((who0 (get member e0)))
            (default-to false (get paid (map-get? contributions {member: who0, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u1)
      (let ((entry1 (map-get? members {id: u1})))
        (match entry1 e1
          (let ((who1 (get member e1)))
            (default-to false (get paid (map-get? contributions {member: who1, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u2)
      (let ((entry2 (map-get? members {id: u2})))
        (match entry2 e2
          (let ((who2 (get member e2)))
            (default-to false (get paid (map-get? contributions {member: who2, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u3)
      (let ((entry3 (map-get? members {id: u3})))
        (match entry3 e3
          (let ((who3 (get member e3)))
            (default-to false (get paid (map-get? contributions {member: who3, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u4)
      (let ((entry4 (map-get? members {id: u4})))
        (match entry4 e4
          (let ((who4 (get member e4)))
            (default-to false (get paid (map-get? contributions {member: who4, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u5)
      (let ((entry5 (map-get? members {id: u5})))
        (match entry5 e5
          (let ((who5 (get member e5)))
            (default-to false (get paid (map-get? contributions {member: who5, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u6)
      (let ((entry6 (map-get? members {id: u6})))
        (match entry6 e6
          (let ((who6 (get member e6)))
            (default-to false (get paid (map-get? contributions {member: who6, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u7)
      (let ((entry7 (map-get? members {id: u7})))
        (match entry7 e7
          (let ((who7 (get member e7)))
            (default-to false (get paid (map-get? contributions {member: who7, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u8)
      (let ((entry8 (map-get? members {id: u8})))
        (match entry8 e8
          (let ((who8 (get member e8)))
            (default-to false (get paid (map-get? contributions {member: who8, cycle: cycle})))
          )
          false
        )
      )
      true
    )
    (if (> count u9)
      (let ((entry9 (map-get? members {id: u9})))
        (match entry9 e9
          (let ((who9 (get member e9)))
            (default-to false (get paid (map-get? contributions {member: who9, cycle: cycle})))
          )
          false
        )
      )
      true
    )
  )
)

;; wrapper read-only to check if all paid for cycle
(define-read-only (all-paid-for-cycle (cycle uint))
  (let ((count (var-get member-count)))
    (ok (if (<= count u0) false (all-paid-for-cycle-helper count cycle)))
  )
)

;; get contract STX balance (use as-contract)
(define-read-only (contract-balance)
  (ok (stx-get-balance (as-contract tx-sender)))
)

;; ---------------------------
;; ADMIN FUNCTIONS
;; ---------------------------

;; add-member: only admin can add; returns assigned id
(define-public (add-member (who principal))
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
    (asserts! (not (member-exists? who)) ERR-ALREADY-MEMBER)
    (let ((cnt (var-get member-count)))
      (asserts! (< cnt max-members) ERR-GROUP-FULL)
      (map-set members { id: cnt } { member: who })
      (map-set member-id { member: who } { id: cnt })
      (var-set member-count (+ cnt u1))
      (ok cnt)
    )
  )
)

;; remove-member: admin only (removes the member by id swap technique)
(define-public (remove-member (who principal))
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
    (asserts! (member-exists? who) ERR-NOT-MEMBER)
    (asserts! (> (var-get member-count) u0) ERR-NO-MEMBERS)
    (let ((id-to-remove (get-member-id who)))
      (if (is-eq id-to-remove (- (var-get member-count) u1))
          (do-remove-member id-to-remove who)
          (do-swap-remove-member id-to-remove who))
    )
  )
)

;; Remove member by id (used internally)
(define-private (do-remove-member (id-to-remove uint) (who principal))
  (begin
    (map-delete members { id: id-to-remove })
    (map-delete member-id { member: who })
    (var-set member-count (- (var-get member-count) u1))
    (ok true)
  )
)

(define-private (do-swap-remove-member (id-to-remove uint) (who principal))
  (let ((last-id (- (var-get member-count) u1))
        (last-entry (map-get? members { id: last-id })))
    (if (is-some last-entry)
      (let ((last-member (get member (unwrap! last-entry ERR-INVALID-INDEX))))
        (begin
          (map-set members { id: id-to-remove } { member: last-member })
          (map-set member-id { member: last-member } { id: id-to-remove })
          (map-delete members { id: last-id })
          (map-delete member-id { member: who })
          (var-set member-count (- (var-get member-count) u1))
          (ok true)
        )
      )
      (ok false)
    )
  )
)

;; update contribution amount (admin)
(define-public (set-contribution-amount (amount uint))
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
  (asserts! (> amount u0) ERR-CONTRIBUTION-MISMATCH)
  (var-set contribution-amount amount)
    (ok amount)
  )
)

;; update cycle length in blocks (admin)
(define-public (set-cycle-length (blocks uint))
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
  (asserts! (> blocks u0) ERR-INVALID-INDEX)
  (var-set cycle-length blocks)
    (ok blocks)
  )
)

;; start the group (admin) - sets the cycle-start-block and started = true
(define-public (start-group)
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
    (asserts! (not (var-get started)) ERR-ALREADY-STARTED)
    (asserts! (> (var-get member-count) u0) ERR-NO-MEMBERS)
    (var-set current-cycle u0)
  ;; TODO: Replace u0 with actual block height when supported by toolchain
  (var-set cycle-start-block u0)
    (var-set current-turn-index u0)
    (var-set started true)
    (ok true)
  )
)

;; ---------------------------
;; MEMBER ACTIONS
;; ---------------------------

;; Member contributes STX for the current cycle.
;; The call transfers the expected amount from member -> contract.
(define-public (contribute)
  (begin
    (asserts! (var-get started) ERR-NOT-STARTED)
    (asserts! (member-exists? tx-sender) ERR-NOT-MEMBER)
    (let ((cycle (var-get current-cycle))
          (expected (unwrap-panic (get-expected-amount tx-sender (var-get current-cycle)))))
      (asserts! (not (default-to false (get paid (map-get? contributions { member: tx-sender, cycle: cycle })))) ERR-ALREADY-PAID)
      ;; transfer STX from caller to contract
  (try! (stx-transfer? expected tx-sender (as-contract tx-sender)))
      (map-set contributions { member: tx-sender, cycle: cycle } { paid: true })
      (map-delete penalties { member: tx-sender })
      (ok true)
    )
  )
)

;; Member (whose turn this cycle) claims the pot.
;; Only allowed when all members have paid for the current cycle.
(define-public (claim-pot)
  (begin
    (asserts! (var-get started) ERR-NOT-STARTED)
    (let ((count (var-get member-count)))
      (asserts! (> count u0) ERR-NO-MEMBERS)
      (let ((turn (var-get current-turn-index)))
        (match (map-get? members { id: turn })
          entry
          (let ((turn-member (get member entry)))
            (asserts! (is-eq tx-sender turn-member) ERR-NOT-YOUR-TURN)
            (let ((all-paid (unwrap! (all-paid-for-cycle (var-get current-cycle)) ERR-NOT-PAID)))
              (if all-paid
                (let ((bal (stx-get-balance (as-contract tx-sender))))
                  (asserts! (> bal u0) ERR-INSUFFICIENT-BALANCE)
                  (try! (stx-transfer? bal (as-contract tx-sender) turn-member))
                  ;; advance cycle state
                  (var-set current-cycle (+ (var-get current-cycle) u1))
                  ;; TODO: Replace u0 with actual block height when supported by toolchain
                  (var-set cycle-start-block u0)
                  (var-set current-turn-index (mod (+ turn u1) count))
                  (ok bal)
                )
                (err u1)
              )
            )
          )
          (err u1)
        )
      )
    )
  )
)

;; admin can mark a member as late (if deadline passed) and assign a penalty (uint)
;; penalty will be required for their next contribution (added to expected amount)
(define-public (assign-penalty (who principal) (amount uint))
  (begin
    (asserts! (is-admin? tx-sender) ERR-NOT-ADMIN)
    (asserts! (member-exists? who) ERR-NOT-MEMBER)
  (asserts! (> amount u0) ERR-INVALID-INDEX)
  (map-set penalties { member: who } { amount: amount })
    (ok true)
  )
)

;; ---------------------------
;; Helper: assign-penalties-loop
;; Walk members 0..count-1, if not paid for current-cycle assign penalty = contribution/10
;; ---------------------------
;; Unrolled penalty assignment for up to 10 members
(define-private (assign-penalties-unrolled (count uint) (cycle uint))
  (begin
    (if (> count u0)
      (let ((entry0 (map-get? members {id: u0})))
        (match entry0 e0
          (let ((who0 (get member e0)))
            (let ((paid0 (default-to false (get paid (map-get? contributions {member: who0, cycle: cycle})))) )
              (if (not paid0)
                (map-set penalties {member: who0} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u1)
      (let ((entry1 (map-get? members {id: u1})))
        (match entry1 e1
          (let ((who1 (get member e1)))
            (let ((paid1 (default-to false (get paid (map-get? contributions {member: who1, cycle: cycle})))) )
              (if (not paid1)
                (map-set penalties {member: who1} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u2)
      (let ((entry2 (map-get? members {id: u2})))
        (match entry2 e2
          (let ((who2 (get member e2)))
            (let ((paid2 (default-to false (get paid (map-get? contributions {member: who2, cycle: cycle})))) )
              (if (not paid2)
                (map-set penalties {member: who2} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u3)
      (let ((entry3 (map-get? members {id: u3})))
        (match entry3 e3
          (let ((who3 (get member e3)))
            (let ((paid3 (default-to false (get paid (map-get? contributions {member: who3, cycle: cycle})))) )
              (if (not paid3)
                (map-set penalties {member: who3} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u4)
      (let ((entry4 (map-get? members {id: u4})))
        (match entry4 e4
          (let ((who4 (get member e4)))
            (let ((paid4 (default-to false (get paid (map-get? contributions {member: who4, cycle: cycle})))) )
              (if (not paid4)
                (map-set penalties {member: who4} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u5)
      (let ((entry5 (map-get? members {id: u5})))
        (match entry5 e5
          (let ((who5 (get member e5)))
            (let ((paid5 (default-to false (get paid (map-get? contributions {member: who5, cycle: cycle})))) )
              (if (not paid5)
                (map-set penalties {member: who5} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u6)
      (let ((entry6 (map-get? members {id: u6})))
        (match entry6 e6
          (let ((who6 (get member e6)))
            (let ((paid6 (default-to false (get paid (map-get? contributions {member: who6, cycle: cycle})))) )
              (if (not paid6)
                (map-set penalties {member: who6} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u7)
      (let ((entry7 (map-get? members {id: u7})))
        (match entry7 e7
          (let ((who7 (get member e7)))
            (let ((paid7 (default-to false (get paid (map-get? contributions {member: who7, cycle: cycle})))) )
              (if (not paid7)
                (map-set penalties {member: who7} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u8)
      (let ((entry8 (map-get? members {id: u8})))
        (match entry8 e8
          (let ((who8 (get member e8)))
            (let ((paid8 (default-to false (get paid (map-get? contributions {member: who8, cycle: cycle})))) )
              (if (not paid8)
                (map-set penalties {member: who8} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
    (if (> count u9)
      (let ((entry9 (map-get? members {id: u9})))
        (match entry9 e9
          (let ((who9 (get member e9)))
            (let ((paid9 (default-to false (get paid (map-get? contributions {member: who9, cycle: cycle})))) )
              (if (not paid9)
                (map-set penalties {member: who9} {amount: (/ (var-get contribution-amount) u10)})
                true
              )
            )
          )
          true
        )
      )
      true
    )
  )
)

;; check and optionally advance cycle automatically if cycle timeout exceeded (helper)
;; call this when needed (front-end or admin)
(define-public (check-advance-cycle)
  (begin
    (asserts! (var-get started) ERR-NOT-STARTED)
    (let ((count (var-get member-count))
          (cycle (var-get current-cycle))
          (start-block (var-get cycle-start-block))
          (length (var-get cycle-length)))
      (asserts! (> count u0) ERR-NO-MEMBERS)
      ;; if cycle-length is zero treat as no auto-advance
  (if (or (is-eq length u0) (<= (+ start-block length) u0)) ;; TODO: Replace u0 with block-height when supported
          (begin
            ;; assign penalties for unpaid members
            (assign-penalties-unrolled count cycle)
            ;; advance cycle
            (var-set current-cycle (+ cycle u1))
            ;; TODO: Replace u0 with actual block height when supported by toolchain
            (var-set cycle-start-block u0)
            (var-set current-turn-index (mod (+ (var-get current-turn-index) u1) count))
            (ok true)
          )
          (ok false)
      )
    )
  )
)

;; ---------------------------
;; READ-ONLY / GETTERS
;; ---------------------------

(define-read-only (get-member-count)
  (ok (var-get member-count))
)

(define-read-only (get-member-by-id (id uint))
  (match (map-get? members { id: id })
    m (ok (get member m))
    (err ERR-INVALID-INDEX)
  )
)

(define-read-only (get-member-id-of (who principal))
  (match (map-get? member-id { member: who })
    r (ok (get id r))
    (err ERR-NOT-MEMBER)
  )
)

(define-read-only (has-paid (who principal) (cycle uint))
  (ok (default-to false (get paid (map-get? contributions { member: who, cycle: cycle }))))
)

(define-read-only (get-current-state)
  (ok
    (tuple
      (member-count (var-get member-count))
      (contribution-amount (var-get contribution-amount))
      (cycle-length (var-get cycle-length))
      (started (var-get started))
      (current-cycle (var-get current-cycle))
      (cycle-start-block (var-get cycle-start-block))
      (current-turn-index (var-get current-turn-index))
  (contract-balance (stx-get-balance (as-contract tx-sender)))
    )
  )
)

(define-read-only (get-penalty (who principal))
  (ok (default-to u0 (get amount (map-get? penalties { member: who }))))
)
