
;; Educhain-logic
;; Educational Achievement and Reward Management System
;; A comprehensive smart contract for managing educational achievements, 
;; certifications, and rewards on the Stacks blockchain

;; constants
(define-constant CONTRACT-OWNER (as-contract tx-sender))
(define-constant MAX-ACHIEVEMENT-NAME-LENGTH 100)
(define-constant MAX-DESCRIPTION-LENGTH 500)
(define-constant MAX-CATEGORY-LENGTH 50)
(define-constant MIN-REWARD-AMOUNT u1000)
(define-constant MAX-REWARD-AMOUNT u1000000)
(define-constant MAX-ACHIEVEMENTS-PER-USER 100)
(define-constant MAX-CERTIFICATIONS-PER-USER 50)

;; error codes
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-INPUT (err u1002))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u1003))
(define-constant ERR-USER-NOT-FOUND (err u1004))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u1005))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1006))
(define-constant ERR-LIMIT-EXCEEDED (err u1007))
(define-constant ERR-CERTIFICATION-NOT-FOUND (err u1008))

;; data maps and vars
;; Achievement definitions - what achievements exist and their requirements
(define-map achievement-definitions
  (id uint)
  (tuple 
    (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
    (description (string-ascii MAX-DESCRIPTION-LENGTH))
    (category (string-ascii MAX-CATEGORY-LENGTH))
    (reward-amount uint)
    (issuer principal)
    (active bool)
    (created-at uint)
  )
)

;; User achievements - tracking which users have earned which achievements
(define-map user-achievements
  (tuple (user principal) (achievement-id uint))
  (tuple 
    (earned-at uint)
    (claimed bool)
    (issuer principal)
  )
)

;; User profiles - basic user information and statistics
(define-map user-profiles
  (user principal)
  (tuple 
    (total-achievements uint)
    (total-rewards-claimed uint)
    (total-points uint)
    (joined-at uint)
    (last-activity uint)
  )
)

;; Certifications - special achievements that grant certifications
(define-map certifications
  (id uint)
  (tuple 
    (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
    (description (string-ascii MAX-DESCRIPTION-LENGTH))
    (required-achievements (list uint))
    (issuer principal)
    (active bool)
    (created-at uint)
  )
)

;; User certifications - tracking which users have earned certifications
(define-map user-certifications
  (tuple (user principal) (certification-id uint))
  (tuple 
    (earned-at uint)
    (issuer principal)
  )
)

;; Issuer registry - authorized issuers who can create achievements
(define-map authorized-issuers
  (issuer principal)
  (tuple 
    (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
    (description (string-ascii MAX-DESCRIPTION-LENGTH))
    (active bool)
    (registered-at uint)
  )
)

;; Contract state variables
(define-data-var total-achievements uint u0)
(define-data-var total-certifications uint u0)
(define-data-var total-users uint u0)
(define-data-var contract-balance uint u0)
(define-data-var contract-paused bool false)

;; private functions
;;

;; Helper function to check if contract is paused
(define-private (is-contract-paused)
  (var-get contract-paused)
)

;; Helper function to check if caller is contract owner
(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

;; Helper function to check if caller is authorized issuer
(define-private (is-authorized-issuer (issuer principal))
  (match (map-get? authorized-issuers issuer)
    issuer-data (get active issuer-data)
    false
  )
)

;; Helper function to validate string length
(define-private (validate-string-length (input (string-ascii 500)) (max-length uint))
  (<= (len input) max-length)
)

;; Helper function to validate reward amount
(define-private (validate-reward-amount (amount uint))
  (and 
    (>= amount MIN-REWARD-AMOUNT)
    (<= amount MAX-REWARD-AMOUNT)
  )
)

;; Helper function to get current block time
(define-private (get-current-time)
  block-height
)

;; Helper function to create or update user profile
(define-private (create-or-update-user-profile (user principal))
  (let ((current-time (get-current-time)))
    (match (map-get? user-profiles user)
      existing-profile 
      (map-set user-profiles user 
        (merge existing-profile 
          (tuple (last-activity current-time))
        )
      )
      ;; Create new profile
      (begin
        (map-set user-profiles user 
          (tuple 
            (total-achievements u0)
            (total-rewards-claimed u0)
            (total-points u0)
            (joined-at current-time)
            (last-activity current-time)
          )
        )
        (var-set total-users (+ (var-get total-users) u1))
      )
    )
  )
)

;; Helper function to check if user has achievement
(define-private (user-has-achievement (user principal) (achievement-id uint))
  (is-some (map-get? user-achievements (tuple (user user) (achievement-id achievement-id))))
)

;; Helper function to check if user has certification
(define-private (user-has-certification (user principal) (certification-id uint))
  (is-some (map-get? user-certifications (tuple (user user) (certification-id certification-id))))
)

;; Helper function to get user achievement data
(define-private (get-user-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements (tuple (user user) (achievement-id achievement-id)))
)

;; Helper function to get achievement definition
(define-private (get-achievement-definition (achievement-id uint))
  (map-get? achievement-definitions achievement-id)
)

;; Helper function to get certification definition
(define-private (get-certification-definition (certification-id uint))
  (map-get? certifications certification-id)
)

;; Helper function to check if user meets certification requirements
(define-private (user-meets-certification-requirements (user principal) (required-achievements (list uint)))
  (fold check-achievement-requirement required-achievements true)
)

;; Helper function to check individual achievement requirement
(define-private (check-achievement-requirement (achievement-id uint) (all-met bool))
  (and all-met (user-has-achievement tx-sender achievement-id))
)

;; Helper function to calculate total points for user
(define-private (calculate-user-points (user principal))
  (fold calculate-achievement-points (get-user-achievement-ids user) u0)
)

;; Helper function to get all achievement IDs for a user
;; Note: This is a simplified version since Clarity doesn't have map-keys
;; In practice, you would need to track this separately or use a different approach
(define-private (get-user-achievement-ids (user principal))
  (list u0) ;; Placeholder - would need separate tracking in real implementation
)

;; Helper function to extract achievement ID from map key
(define-private (get-achievement-id-from-key (key (tuple (user principal) (achievement-id uint))))
  (get achievement-id key)
)

;; Helper function to calculate points for a single achievement
(define-private (calculate-achievement-points (achievement-id uint) (total-points uint))
  (match (get-achievement-definition achievement-id)
    achievement-def (let ((reward-amount (get reward-amount achievement-def)))
      (+ total-points reward-amount)
    )
    total-points
  )
)

;; Helper function to validate achievement input
(define-private (validate-achievement-input 
  (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
  (description (string-ascii MAX-DESCRIPTION-LENGTH))
  (category (string-ascii MAX-CATEGORY-LENGTH))
  (reward-amount uint)
)
  (and
    (validate-string-length name MAX-ACHIEVEMENT-NAME-LENGTH)
    (validate-string-length description MAX-DESCRIPTION-LENGTH)
    (validate-string-length category MAX-CATEGORY-LENGTH)
    (validate-reward-amount reward-amount)
    (not (is-eq name ""))
    (not (is-eq category ""))
  )
)

;; Helper function to validate certification input
(define-private (validate-certification-input
  (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
  (description (string-ascii MAX-DESCRIPTION-LENGTH))
  (required-achievements (list uint))
)
  (and
    (validate-string-length name MAX-ACHIEVEMENT-NAME-LENGTH)
    (validate-string-length description MAX-DESCRIPTION-LENGTH)
    (not (is-eq name ""))
    (> (len required-achievements) u0)
  )
)

;; Helper function to check if user has reached achievement limit
(define-private (user-achievement-limit-reached (user principal))
  (match (map-get? user-profiles user)
    profile (>= (get total-achievements profile) MAX-ACHIEVEMENTS-PER-USER)
    false
  )
)

;; Helper function to check if user has reached certification limit
(define-private (user-certification-limit-reached (user principal))
  (let ((certification-count (len (get-user-certification-ids user))))
    (>= certification-count MAX-CERTIFICATIONS-PER-USER)
  )
)

;; Helper function to get all certification IDs for a user
;; Note: This is a simplified version since Clarity doesn't have map-keys
;; In practice, you would need to track this separately or use a different approach
(define-private (get-user-certification-ids (user principal))
  (list u0) ;; Placeholder - would need separate tracking in real implementation
)

;; Helper function to extract certification ID from map key
(define-private (get-certification-id-from-key (key (tuple (user principal) (certification-id uint))))
  (get certification-id key)
)

;; public functions
;;

;; ===== CONTRACT ADMINISTRATION FUNCTIONS =====

;; Pause or unpause the contract (owner only)
(define-public (set-contract-paused (paused bool))
  (begin
    (assert! (is-owner) ERR-UNAUTHORIZED)
    (assert! (not (is-contract-paused)) ERR-INVALID-INPUT)
    (var-set contract-paused paused)
    (ok true)
  )
)

;; Register a new authorized issuer (owner only)
(define-public (register-issuer 
  (issuer principal)
  (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
  (description (string-ascii MAX-DESCRIPTION-LENGTH))
)
  (begin
    (asserts (is-owner) ERR-UNAUTHORIZED)
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts (validate-string-length name MAX-ACHIEVEMENT-NAME-LENGTH) ERR-INVALID-INPUT)
    (asserts (validate-string-length description MAX-DESCRIPTION-LENGTH) ERR-INVALID-INPUT)
    (asserts (not (is-eq name "")) ERR-INVALID-INPUT)
    (map-set authorized-issuers issuer 
      (tuple 
        (name name)
        (description description)
        (active true)
        (registered-at (get-current-time))
      )
    )
    (ok true)
  )
)

;; Deactivate an authorized issuer (owner only)
(define-public (deactivate-issuer (issuer principal))
  (begin
    (asserts (is-owner) ERR-UNAUTHORIZED)
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (map-get? authorized-issuers issuer)
      issuer-data 
      (map-set authorized-issuers issuer 
        (merge issuer-data (tuple (active false)))
      )
      (err ERR-INVALID-INPUT)
    )
    (ok true)
  )
)

;; ===== ACHIEVEMENT MANAGEMENT FUNCTIONS =====

;; Create a new achievement (authorized issuers only)
(define-public (create-achievement
  (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
  (description (string-ascii MAX-DESCRIPTION-LENGTH))
  (category (string-ascii MAX-CATEGORY-LENGTH))
  (reward-amount uint)
)
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts (validate-achievement-input name description category reward-amount) ERR-INVALID-INPUT)
    (let ((new-achievement-id (+ (var-get total-achievements) u1)))
      (map-set achievement-definitions new-achievement-id
        (tuple 
          (name name)
          (description description)
          (category category)
          (reward-amount reward-amount)
          (issuer tx-sender)
          (active true)
          (created-at (get-current-time))
        )
      )
      (var-set total-achievements new-achievement-id)
      (ok new-achievement-id)
    )
  )
)

;; Award an achievement to a user (authorized issuers only)
(define-public (award-achievement
  (user principal)
  (achievement-id uint)
)
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts (not (user-has-achievement user achievement-id)) ERR-INVALID-INPUT)
    (asserts (not (user-achievement-limit-reached user)) ERR-LIMIT-EXCEEDED)
    (match (get-achievement-definition achievement-id)
      achievement-def 
      (begin
        (asserts (get active achievement-def) ERR-ACHIEVEMENT-NOT-FOUND)
        (create-or-update-user-profile user)
        (map-set user-achievements (tuple (user user) (achievement-id achievement-id))
          (tuple 
            (earned-at (get-current-time))
            (claimed false)
            (issuer tx-sender)
          )
        )
        ;; Update user profile statistics
        (match (map-get? user-profiles user)
          profile 
          (map-set user-profiles user 
            (merge profile 
              (tuple 
                (total-achievements (+ (get total-achievements profile) u1))
                (total-points (+ (get total-points profile) (get reward-amount achievement-def)))
              )
            )
          )
        )
        (ok true)
      )
      (err ERR-ACHIEVEMENT-NOT-FOUND)
    )
  )
)

;; Deactivate an achievement (issuer or owner only)
(define-public (deactivate-achievement (achievement-id uint))
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (get-achievement-definition achievement-id)
      achievement-def 
      (begin
        (asserts (or (is-owner) (is-eq tx-sender (get issuer achievement-def))) ERR-UNAUTHORIZED)
        (map-set achievement-definitions achievement-id
          (merge achievement-def (tuple (active false)))
        )
        (ok true)
      )
      (err ERR-ACHIEVEMENT-NOT-FOUND)
    )
  )
)

;; ===== CERTIFICATION MANAGEMENT FUNCTIONS =====

;; Create a new certification (authorized issuers only)
(define-public (create-certification
  (name (string-ascii MAX-ACHIEVEMENT-NAME-LENGTH))
  (description (string-ascii MAX-DESCRIPTION-LENGTH))
  (required-achievements (list uint))
)
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts (validate-certification-input name description required-achievements) ERR-INVALID-INPUT)
    (let ((new-certification-id (+ (var-get total-certifications) u1)))
      (map-set certifications new-certification-id
        (tuple 
          (name name)
          (description description)
          (required-achievements required-achievements)
          (issuer tx-sender)
          (active true)
          (created-at (get-current-time))
        )
      )
      (var-set total-certifications new-certification-id)
      (ok new-certification-id)
    )
  )
)

;; Award a certification to a user (authorized issuers only)
(define-public (award-certification
  (user principal)
  (certification-id uint)
)
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (asserts (is-authorized-issuer tx-sender) ERR-UNAUTHORIZED)
    (asserts (not (user-has-certification user certification-id)) ERR-INVALID-INPUT)
    (asserts (not (user-certification-limit-reached user)) ERR-LIMIT-EXCEEDED)
    (match (get-certification-definition certification-id)
      certification-def 
      (begin
        (asserts (get active certification-def) ERR-CERTIFICATION-NOT-FOUND)
        (asserts (user-meets-certification-requirements user (get required-achievements certification-def)) ERR-INVALID-INPUT)
        (create-or-update-user-profile user)
        (map-set user-certifications (tuple (user user) (certification-id certification-id))
          (tuple 
            (earned-at (get-current-time))
            (issuer tx-sender)
          )
        )
        (ok true)
      )
      (err ERR-CERTIFICATION-NOT-FOUND)
    )
  )
)

;; Deactivate a certification (issuer or owner only)
(define-public (deactivate-certification (certification-id uint))
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (get-certification-definition certification-id)
      certification-def 
      (begin
        (asserts (or (is-owner) (is-eq tx-sender (get issuer certification-def))) ERR-UNAUTHORIZED)
        (map-set certifications certification-id
          (merge certification-def (tuple (active false)))
        )
        (ok true)
      )
      (err ERR-CERTIFICATION-NOT-FOUND)
    )
  )
)

;; ===== REWARD MANAGEMENT FUNCTIONS =====

;; Claim reward for an achievement (user only)
(define-public (claim-achievement-reward (achievement-id uint))
  (begin
    (asserts (not (is-contract-paused)) ERR-INVALID-INPUT)
    (match (get-user-achievement tx-sender achievement-id)
      user-achievement 
      (begin
        (asserts (not (get claimed user-achievement)) ERR-REWARD-ALREADY-CLAIMED)
        (match (get-achievement-definition achievement-id)
          achievement-def 
          (begin
            (asserts (get active achievement-def) ERR-ACHIEVEMENT-NOT-FOUND)
            (let ((reward-amount (get reward-amount achievement-def)))
              (asserts (>= (var-get contract-balance) reward-amount) ERR-INSUFFICIENT-BALANCE)
              ;; Mark as claimed
              (map-set user-achievements (tuple (user tx-sender) (achievement-id achievement-id))
                (merge user-achievement (tuple (claimed true)))
              )
              ;; Update user profile
              (match (map-get? user-profiles tx-sender)
                profile 
                (map-set user-profiles tx-sender 
                  (merge profile 
                    (tuple (total-rewards-claimed (+ (get total-rewards-claimed profile) reward-amount)))
                  )
                )
              )
              ;; Update contract balance
              (var-set contract-balance (- (var-get contract-balance) reward-amount))
              ;; Transfer reward to user (in a real implementation, this would use stx-transfer)
              (ok reward-amount)
            )
          )
          (err ERR-ACHIEVEMENT-NOT-FOUND)
        )
      )
      (err ERR-ACHIEVEMENT-NOT-FOUND)
    )
  )
)

;; ===== QUERY FUNCTIONS =====

;; Get user profile information
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

;; Get achievement definition
(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievement-definitions achievement-id)
)

;; Get certification definition
(define-read-only (get-certification (certification-id uint))
  (map-get? certifications certification-id)
)

;; Check if user has specific achievement
(define-read-only (has-achievement (user principal) (achievement-id uint))
  (user-has-achievement user achievement-id)
)

;; Check if user has specific certification
(define-read-only (has-certification (user principal) (certification-id uint))
  (user-has-certification user certification-id)
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  (tuple 
    (total-achievements (var-get total-achievements))
    (total-certifications (var-get total-certifications))
    (total-users (var-get total-users))
    (contract-balance (var-get contract-balance))
    (contract-paused (var-get contract-paused))
  )
)
