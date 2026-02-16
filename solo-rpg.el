;;; solo-rpg.el --- Solo roleplaying games support functions  -*- lexical-binding: t; -*-

;; Author: Christer Enfors <christer.enfors@gmail.com>
;; Maintainer: Christer Enfors <christer.enfors@gmail.com>
;; Created: 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, convenience, wp
;; URL: https://github.com/enfors/lonelog

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
;;
;; To use this package, add the following to your configuration:
;;
;;   (require 'solo-rpg)
;;
;; Customization:

;;; Code:

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
  "Take DICE-STRING, parse it, make the roll, and return a user-displayable result."
  (solo-rpg-dice-roll-string (solo-rpg-dice-roll-calculate (solo-rpg-dice-string-parse
                                                            dice-string))))

(defun solo-rpg-dice-roll-message (dice-string)
  "Ask for DICE-STRING, roll the dice, display result in message window."
  (interactive "sEnter dice string (for example '2d6+2'): ")
  (message (solo-rpg-dice-roll-cast dice-string)))

(defun solo-rpg-dice-roll-insert (dice-string)
  "Ask for DICE-STRING, roll the dice, output the result in the current buffer."
  (interactive "sEnter dice string (for example '2d6+2'): ")
  (insert (solo-rpg-dice-roll-cast dice-string)))
