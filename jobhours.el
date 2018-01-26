;;; jobhours --- Companion code for the 'hours' Haskell utility

;; Copyright (C) 2018 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Created: 12 Dec 2012
;; Version: 3.0
;; Keywords: hours
;; X-URL: https://github.com/jwiegley/hours

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code calls out to the 'hours' utility and provides a UI display.

(defgroup jobhours nil
  "Companion code for the 'hours' Haskell utility"
  :group 'jobhours)

(defcustom jobhours-file "todo.txt"
  "The Org-file from which time clock data is read.
Note that the 'org2tc' utility must be on your PATH."
  :type 'file
  :group 'jobhours)

(defun jobhours-dim-color (color cloudiness)
  (cl-destructuring-bind (h s l)
      (apply 'color-rgb-to-hsl (color-name-to-rgb color))
    (apply 'color-rgb-to-hex
           (color-hsl-to-rgb h (* s (min 1 cloudiness)) l))))

(defun jobhours-make-text-properties (logged-in disc dir &optional color)
  (list :background (or color
                        (jobhours-dim-color (if (< disc 0)
                                                "#ffff00000000"
                                              "#0000ffff0000")
                                            (/ (abs disc) 8.0)))
        :foreground (if logged-in
                        (if (< dir 0)
                            "#0000ffffffff"
                          "yellow")
                      "#000000000000")
        :weight (if logged-in 'bold 'light)))

(defun get-jobhours-string ()
  (with-temp-buffer
    (call-process "jobhours" nil t nil (expand-file-name jobhours-file))

    (goto-char (point-min))
    (let* ((details (read (current-buffer)))
           (logged-in            (alist-get 'logged-in details))
           (current-period       (alist-get 'current-period details))
           (ideal-total          (alist-get 'ideal-total details))
           (real-completed       (alist-get 'real-completed details))
           (real-expected        (alist-get 'real-expected details))
           (real-expected-inact  (alist-get 'real-expected-inact details))
           (real-this-remaining  (alist-get 'real-this-remaining details))
           (real-discrepancy     (alist-get 'real-discrepancy details)))

      (delete-region (point-min) (point-max))
      (insert "  " (format "%s%.1fh %s %.1f"
                           (if (< real-this-remaining 0) "+" "")
                           (abs real-this-remaining)
                           (pcase current-period
                             (`Holiday    "?")
                             (`OffFriday  "!")
                             (`HalfFriday "/")
                             (`RegularDay "|")
                             (`NotWorking "∙"))
                           (min real-expected real-expected-inact)) "  ")

      ;; Color the whole "time bar" a neutral, light grey
      (add-face-text-property (point-min) (point-max) '(:background "grey75"))

      ;; Now darken a percentage of the bar, starting from the left, to show
      ;; what percentage of the time period has been worked.
      (add-face-text-property
       (max 1
            (- (point-max) (floor (* (point-max)
                                     (/ real-completed ideal-total)))))
       (point-max)

       ;; If our moving average is above or below nominal, shade the darker
       ;; area toward green or red. The further from nominal, the more intense
       ;; the color becomes, reaching full intensity at 1 work day.
       (jobhours-make-text-properties logged-in real-discrepancy
                                      real-this-remaining))

      (buffer-string))))

(defvar jobhours-string "")
(put 'jobhours-string 'risky-local-variable t)

(defun jobhours-update-string ()
  (setq jobhours-string (get-jobhours-string)))

(defun jobhours-setup-modeline ()
  (run-at-time 0 300 #'jobhours-update-string)
  (push " " (default-value 'mode-line-format))
  (push '(:eval jobhours-string) (default-value 'mode-line-format)))

(provide 'jobhours)
