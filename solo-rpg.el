;;; solo-rpg.el --- Solo roleplaying games support functions  -*- lexical-binding: t; -*-

;; Author: Christer Enfors <christer.enfors@gmail.com>
;; Maintainer: Christer Enfors <christer.enfors@gmail.com>
;; Created: 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, convenience, wp
;; URL: https://github.com/enfors/solo-rpg

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Solo-rpg is a set up support functions for playing solo roleplaying games
;; in Emacs.

;; Features include:
;; - Dice rolling
;; - Oracles:
;;   - Yes/No Oracle with probabilities
;;   - Action/Theme Oracle, inspired by the Mything Game Master Emulator
;;
;; To use this package, add the following to your configuration:
;;
;;   (require 'solo-rpg)
;;   (with-eval-after-load 'solo-rpg
;;     ;; Note - you can replace "C-c r" with another key if you prefer
;;     (define-key solo-rpg-mode-map (kbd "C-c r") 'solo-rpg-menu)
;;
;; To start solo-rpg-mode, type:
;;
;;   M-x solo-rpg-mode
;;
;; Once solo-rpg-mode is started, you can invoke the dashboard menu by
;; typing the shortcut key you chose above ("C-c r" by default).
;;
;; Customization:
;;
;;   `solo-rpg-output-method' can be set to 'insert or 'message.
;;      - If set to 'message, output will be sent to message buffer only.
;;      - If set to 'insert, output will also be inserted in current buffer.

;;; Code:

;;; Configuration variables:

(defcustom solo-rpg-output-method 'insert
  "Default method for outputting solo-rpg results.
Can be 'insert to put text in current buffer, or 'message to only echo it."
  :type '(choice (const :tag "Insert in current buffer" insert)
                 (const :tag "Only show in message area" message))
  :group 'solo-rpg)

;;; Utility functions:

(defun solo-rpg--output (text &optional invert-behavior)
  "Output TEXT according to `solo-rpg-output-method'.
If INVERT-BEHAVIOR is non-nil, do the opposite of the default setting."
  ;; Always log it to the message area.
  (message "%s" text)

  ;; Determine if we should insert based on setting and override
  (let ((should-insert (eq solo-rpg-output-method 'insert)))
    (when invert-behavior
      (setq should-insert (not should-insert)))

    ;; Insert if needed
    (when should-insert
      (solo-rpg--space-insert text))))

(defun solo-rpg--space-insert (&rest args)
  "Like `insert' but prepends a space of preceding character isn't whitespace."
  ;; Check if we need a space.
  ;; We ensure that we aren't at the beginning of the buffer (nil),
  ;; and that the previous char isn't a space, newline, or tab.
  (when (and (char-before)
             (not (memq (char-before) '(?\s ?\n ?\t))))
    (insert " "))
  ;; Pass all arguments directly to the standard insert function.
  (apply #'insert args))

;;; Table functions:

(defun solo-rpg-table-get-random (table)
  "Return random element from TABLE."
  (aref table (random (length table))))

(defun solo-rpg--table-weighted-get-random (weighted-table max-value)
  "Return random element from WEIGHTED-TABLE where max value is MAX-VALUE.

Example of a weighted table:
(defconst solo-rpg-oracle-quantity-table
  '((\"Minimum\"     .  1)
    (\"Much less\"   .  3)
    (\"Less\"        .  7)
    (\"As expected\" . 13)
    (\"More\"        . 17)
    (\"Much more\"   . 19)
    (\"Maximum\"     . 20))
  \"Data table for the Quantity oracle.
Values are upper threshold for each entry.\")"
  (let ((roll (+ 1 (random max-value))))
    (cl-loop for (label . threshold) in weighted-table
             if (<= roll threshold)
             return (format "[%d] -> %s" roll label))))

;;; Dice rolls:

(require 'cl-lib)  ;; For structs

(cl-defstruct solo-rpg-dice-roll
  count    ;; Number of dice
  sides    ;; Die sizes
  mod      ;; Modifier (+/-)
  rolls    ;; List of each die roll result
  total)   ;; Sum of all rolls + mod

(defun solo-rpg-dice-string-parse (dice-string)
  "ask for DICE-STRING and parse it, returning a solo-rpg-dice-roll struct."
  (interactive "sEnter dice string (for example 2d6+2): ")
  (unless (string-match "^\\([0-9]+\\)*d\\([0-9]+\\)*\\([+-][0-9]+\\)?"
                        dice-string)
    (user-error "Invalid dice format '%s'. Try '2d6' or '1d20+5'."
                dice-string))
  (let ((count (string-to-number (or (match-string 1 dice-string) "1")))
        (sides (string-to-number (or (match-string 2 dice-string) "6")))
        (mod   (string-to-number (or (match-string 3 dice-string) "0"))))
    ;; Return the struct
    (make-solo-rpg-dice-roll
     :count count
     :sides sides
     :mod mod
     :rolls nil                      ; No rolls have been made yet
     :total nil)))                   ; No totals, because no rolls yet
    
(defun solo-rpg-dice-roll-calculate (roll-struct)
    "Roll the dice in the specified ROLL-STRUCT and fill in the results.
Returns the updated struct."

  ;; Get the data from the struct.
  (let* ((count (solo-rpg-dice-roll-count roll-struct))
         (sides (solo-rpg-dice-roll-sides roll-struct))
         (mod   (solo-rpg-dice-roll-mod   roll-struct))

         ;; Generate random numbers. We use cl-loop to repeat the action
         ;; 'count' times (count = number of dice)
         (results (cl-loop repeat count
                           ;; (random sides returns 0 to sides-1.
                           ;; So we add 1.
                           ;; "collect" is a keyword for the cl-loop macro which
                           ;; builds a list.
                           ;; cl-loop is like a whole language of its own.
                           collect (+ 1 (random sides))))
         ;; Calculate the grand total.
         ;; Sum the results and add the modifier.
         (sum (apply #'+ results))      ; #' is used to quote functions
                                        ; apply is used to step through results
         (grand-total (+ sum mod)))

    ;; Update the struct slots with 'setf' - "set field"
    (setf (solo-rpg-dice-roll-rolls roll-struct) results)
    (setf (solo-rpg-dice-roll-total roll-struct) grand-total)

    ;; Return the updated struct
    roll-struct))

(defun solo-rpg-dice-roll-string (roll-struct)
  "Return a string of the ROLL-STRUCT, meant for displaying to the user."
  (let* ((mod-string (if (not (= 0 (solo-rpg-dice-roll-mod roll-struct)))
                         (format "%+d" (solo-rpg-dice-roll-mod roll-struct))
                       ""))
         (rolls-string (mapconcat (lambda (x) (format "[%d]" x))
                      (solo-rpg-dice-roll-rolls roll-struct)
                      "+")))
    (format "%dd%d%s = %s%s = %d"
            (solo-rpg-dice-roll-count roll-struct)
            (solo-rpg-dice-roll-sides roll-struct)
            mod-string
            rolls-string
            mod-string
            (solo-rpg-dice-roll-total roll-struct))))

(defun solo-rpg-dice-roll-cast (dice-string)
  "Take DICE-STRING, parse it, make the roll, and output the result."
  (interactive "sEnter dice string (for example 2d6+2): ")
  (solo-rpg--output 
   (solo-rpg-dice-roll-string (solo-rpg-dice-roll-calculate (solo-rpg-dice-string-parse
                                                             dice-string)))))

;; (defun solo-rpg-dice-roll-message (dice-string)
;;   "Ask for DICE-STRING, roll the dice, display result in message window."
;;   (interactive "sEnter dice string (for example '2d6+2'): ")
;;   (message (solo-rpg-dice-roll-cast dice-string)))

;; (defun solo-rpg-dice-roll-insert (dice-string)
;;   "Ask for DICE-STRING, roll the dice, output the result in the current buffer."
;;   (interactive "sEnter dice string (for example '2d6+2'): ")
;;   (insert (solo-rpg-dice-roll-cast dice-string)))

;;; Action / Theme oracle:

(defun solo-rpg-oracle-action-theme ()
  "Output '(action) / (theme)' where action and theme are random."
  (interactive)
  (solo-rpg--output (format "%s / %s"
                            (solo-rpg-table-get-random solo-rpg-oracle-actions)
                            (solo-rpg-table-get-random solo-rpg-oracle-themes))))

;;; Yes / No oracle:

(defun solo-rpg-oracle-yes-no-string (odds-string)
  "Return NoAnd, No, NoBut, YesBut, Yes, or YesAnd based on ODDS-STRING."
  (let* ((thresholds (cdr (assoc odds-string solo-rpg-oracle-yes-no-table)))
         (roll (+ 1 (random 20)))
         (result-text nil))
    (setq result-text
          (cond
           ;; Handle random events
           ((= roll 1)
            (format "Random event (negative) related to %s."
                    (solo-rpg-table-get-random solo-rpg-oracle-yes-no-event-subject-table)))
           ((= roll 20)
            (format "Random event (positive) related to %s."
                    (solo-rpg-table-get-random solo-rpg-oracle-yes-no-event-subject-table)))
           ;; Handle ordinary lookups
           ((<= roll (nth 0 thresholds)) "No, and...")
           ((<= roll (nth 1 thresholds)) "No")
           ((<= roll (nth 2 thresholds)) "No, but...")
           ((<= roll (nth 3 thresholds)) "Yes, but...")
           ((<= roll (nth 4 thresholds)) "Yes")
           (t "Yes, and...")))
    ;; Return formatted string.
    ;; We use 'car' on split-string to just show "+3" instead of the full text.
    (format "%s: [%d] -> %s"
            odds-string
            roll
            result-text)))

(defun solo-rpg-oracle-yes-no (odds)
  "Query the Yes/No oracle. 
ODDS is the probability string selected from `solo-rpg-oracle-yes-no-table'."
  (interactive
   (list (completing-read "Probability (Default 50/50): "
                          solo-rpg-oracle-yes-no-table
                          nil t nil nil 
                          "50/50"))) ;; <--- The default value if you hit RET
  
  (let ((result (solo-rpg-oracle-yes-no-string odds)))
    (message "%s" result)
    (solo-rpg--output result)))

;;; Quantity oracle:

(defun solo-rpg-oracle-quantity ()
  "Query the Quantity oracle, displaying the result."
  (interactive)
  (solo-rpg--output (solo-rpg--table-weighted-get-random solo-rpg-oracle-quantity-table 20)))

;;; Transient dashboard

(require 'transient)

(transient-define-prefix solo-rpg-menu-dice ()
  "The solo-rpg Dice menu."
  ["Solo-PRG dashboard: Dice Menu"
   ["Actions"
    ("r" "Roll dice"   solo-rpg-dice-roll-cast)
    ("q" "Go back"     transient-quit-one)]])

;;; Oracle dashboards

(transient-define-prefix solo-rpg-menu-oracle-yes-no-probable ()
  "The solo-rpg menu for the Yes/No Oracle with better than 50/50 probability."
  ["Probability"
   ("6" "Almost certainly"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+6 Almost certainly")))
   ("5" "Highly likely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+5 Highly likely")))
   ("4" "Very likely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+4 Very likely")))
   ("3" "Likely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+3 Likely")))
   ("2" "Probably"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+2 Probably")))
   ("1" "Somewhat likely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "+1 Somewhat likely")))
   ("q" "Go back" transient-quit-one)])

(transient-define-prefix solo-rpg-menu-oracle-yes-no-unprobable ()
  "The solo-rpg menu for the Yes/No Oracle with worse than 50/50 probability."
  ["Probability"
   ("1" "Somewhat unlikely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-1 Somewhat unlikely")))
   ("2" "Probably not"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-2 Probably not")))
   ("3" "Unlikely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-3 Unlikely")))
   ("4" "Very unlikely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-4 Very unlikely")))
   ("5" "Highly unlikely"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-5 Highly unlikely")))
   ("6" "Almost certainly not"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "-6 Almost certainly not")))
   ("q" "Go back" transient-quit-one)])

;; Define the Oracle dashboard menu

(transient-define-prefix solo-rpg-menu-oracle ()
  "The solo-rpg Oracle menu."
  ["Actions"
   ("a" "Action/Theme Oracle"   solo-rpg-oracle-action-theme)
   ("u" "Quantity Oracle"       solo-rpg-oracle-quantity)
   ("q" "Go back"               transient-quit-one)]
  ["Yes/No Oracle"
   ("+" "Better than 50/50 probability" solo-rpg-menu-oracle-yes-no-probable)
   ("0" "50/50 probability"
    (lambda () (interactive) (solo-rpg-oracle-yes-no "50/50")))
   ("-" "Worse than 50/50 probability"   solo-rpg-menu-oracle-yes-no-unprobable)])
   
;; Define the main dashboard menu
(transient-define-prefix solo-rpg-menu ()
  "The main solo-rpg menu."
  ["Solo-RPG dashboard: Main Menu\n"
   ["Actions"
    ("q" "Quit"          transient-quit-one)]
   ["Dice"
    ("d" "Dice menu"     solo-rpg-menu-dice)]
   ["Oracles"
    ("o" "Oracles menu"  solo-rpg-menu-oracle)]])

;;; Minor mode:

;;;###autoload
(defvar solo-rpg-mode-map (make-sparse-keymap)
  "Keymap for `solo-rpg-mode'.
By default, this is empty to allow users to define their own menu key.")

;;;###autoload
(define-minor-mode solo-rpg-mode
  "Minor mode with tools for playing solo roleplaying games.

\\{solo-rpg-mode-map}"
  :init-value nil
  :global nil
  :group 'solo-rpg
  :lighter " SoloRPG"
  :keymap solo-rpg-mode-map

  (if solo-rpg-mode
      ;; If ON:
      (message "Solo-RPG-mode enabled.")
    (message "Solo-RPG-mode disabled.")))


;;; Tables

(defconst solo-rpg-oracle-actions
  ["Abandon"
   "Accept"
   "Accuse"
   "Ambush"
   "Assault"
   "Assist"
   "Attack"
   "Avoid"
   "Balance"
   "Begin"
   "Believe"
   "Betray"
   "Beware"
   "Break"
   "Build"
   "Burn"
   "Cancel"
   "Collect"
   "Collide"
   "Compete"
   "Create"
   "Damage"
   "Deal"
   "Deceive"
   "Decide"
   "Defeat"
   "Defend"
   "Demolish"
   "Deny"
   "Destroy"
   "Detect"
   "Determine"
   "Disable"
   "Dominate"
   "Elaborate"
   "Eliminate"
   "Emerge"
   "Empower"
   "Endanger"
   "Engage"
   "Enhance"
   "Erase"
   "Escape"
   "Fabricate"
   "Fail"
   "Fear"
   "Fight"
   "Flee"
   "Follow"
   "Forget"
   "Fortify"
   "Gain"
   "Grow"
   "Halt"
   "Heal"
   "Hide"
   "Hinder"
   "Hurt"
   "Impersonate"
   "Implicate"
   "Interrupt"
   "Investigate"
   "Keep"
   "Lead"
   "Learn"
   "Leverage"
   "Locate"
   "Mediate"
   "Mislead"
   "Negate"
   "Obey"
   "Observe"
   "Oppress"
   "Promise"
   "Protect"
   "Pursue"
   "Raid"
   "Raise"
   "Recover"
   "Reject"
   "Relocate"
   "Remove"
   "Restrict"
   "Reveal"
   "Ruin"
   "Sabotage"
   "Save"
   "Search"
   "Stop"
   "Talk"
   "Tempt"
   "Terminate"
   "Value"
   "Venture"
   "Verify"
   "Vilify"
   "Violate"
   "Warn"
   "Weaken"
   "Withdraw"]
  "A d100 table of Action words for the Action/Theme oracle.")

(defconst solo-rpg-oracle-themes
  ["Allegations"
   "Alliances"
   "Allies"
   "Ambition"
   "Anger"
   "Artifact"
   "Beginnings"
   "Betrayal"
   "Bonds"
   "Border"
   "Child"
   "Community"
   "Consent"
   "Curse"
   "Darkness"
   "Desecration"
   "Desolation"
   "Devastation"
   "Devotion"
   "Disaster"
   "Discovery"
   "Doom"
   "Dreams"
   "Emotions"
   "Enemies"
   "Fear"
   "Forest"
   "Greatness"
   "Happiness"
   "Hate"
   "Hidden, the"
   "Homes"
   "Hope"
   "Illusion"
   "Innocent"
   "Intrigue"
   "Joy"
   "Kind"
   "Kingdom"
   "Knowledge"
   "Landscape"
   "Language"
   "Leader"
   "Leadership"
   "Legend"
   "Liberty"
   "Lies"
   "Light"
   "Limitations"
   "Loan"
   "Lock"
   "Lord"
   "Lore"
   "Love"
   "Machine"
   "Madness"
   "Magic"
   "Malice"
   "Mania"
   "Master"
   "Medicine"
   "Monsters"
   "Nature"
   "Neglect"
   "Night"
   "Nightmare"
   "Obligation"
   "Oblivion"
   "Occupant"
   "Offense"
   "Opportunity"
   "Opposition"
   "Oppression"
   "Passion"
   "Peace"
   "Plague"
   "Plans"
   "Political, the"
   "Possibilities"
   "Power"
   "Prophet"
   "Quarrel"
   "Realm"
   "Rejection"
   "Reward"
   "Sadness"
   "Science"
   "Secrets"
   "Technology"
   "Terrain"
   "Threat"
   "Traitor"
   "Treason"
   "Truth"
   "Unity"
   "Values"
   "Vandalism"
   "Vicinity"
   "Vision"
   "War"]
  "A d100 table of Theme words for the Action/Theme oracle.")

;;; Yes/No oracle tables

(defconst solo-rpg-oracle-yes-no-table
  '(("+6 Almost certainly"      . (2  3  4  7 16))
    ("+5 Highly likely"         . (2  4  5  8 16))
    ("+4 Very likely"           . (2  5  6  9 17))
    ("+3 Likely"                . (2  6  7  9 17))
    ("+2 Probably"              . (2  6  8 10 17))
    ("+1 Somewhat likely"       . (3  7  9 11 17))
    ("50/50"                    . (3  8 10 12 17))
    ("-1 Somewhat unlikely"     . (3  9 11 13 17))
    ("-2 Probably not"          . (3 10 12 14 18))
    ("-3 Unlikely"              . (3 11 13 14 18))
    ("-4 Very unlikely"         . (3 11 14 15 18))
    ("-5 Highly unlikely"       . (4 12 15 16 18))
    ("-6 Almost certainly not"  . (4 13 16 17 18)))
  "Data table for the Yes/No oracle.
Value is upper thresholds for NoAnd, No, NoBut, YesBut, Yes.")

(defconst solo-rpg-oracle-yes-no-event-subject-table
  ["PC" "NPC" "Faction" "Plot"]
  "Table for random event subjects.")

;;; Quantity oracle table

(defconst solo-rpg-oracle-quantity-table
  '(("Minimum"     .  1)
    ("Much less"   .  3)
    ("Less"        .  7)
    ("As expected" . 13)
    ("More"        . 17)
    ("Much more"   . 19)
    ("Maximum"     . 20))
  "Data table for the Quantity oracle.
Values are upper threshold for each entry.")

(provide 'solo-rpg)

;;; solo-rpg.el ends here
