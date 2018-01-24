(defun jobhours-dim-color (color cloudiness)
  (cl-destructuring-bind (h s l)
      (apply 'color-rgb-to-hsl (color-name-to-rgb color))
    (apply 'color-rgb-to-hex
           (color-hsl-to-rgb h (* s (min 1 cloudiness)) l))))

(defun jobhours-make-text-properties (logged-in disc &optional color)
  (list :background
        (or color
            (jobhours-dim-color (if (< disc 0)
                                    "#ffff00000000"
                                  "#0000ffff0000")
                                (/ (abs disc) 8.0)))
        :foreground
        (if logged-in
            "yellow"
          "#000000000000")
        :weight
        (if logged-in 'bold 'light)))

(defun get-jobhours-string ()
  (with-temp-buffer
    (call-process "jobhours" nil t nil "--emacs")
    (goto-char (point-min))
    (let* ((details (read (current-buffer)))
           (pace (alist-get 'pace-mark details)))

      (delete-region (point-min) (point-max))
      (insert "  " (format "%.1fh | %.1f"
                           (alist-get 'my-today-left details)
                           (alist-get 'my-upcoming-days details))
              "  ")

      ;; Color the whole "time bar" a neutral, light grey
      (add-face-text-property
       (point-min) (point-max)
       (jobhours-make-text-properties
        (alist-get 'logged-in details) pace "grey75"))

      ;; Now darken a percentage of the bar, starting from the left, to show
      ;; what percentage of the time period has been worked.
      (add-face-text-property
       (- (point-max)
          (floor (* (point-max)
                    (/ (alist-get 'my-completed details)
                       (float (alist-get 'work-total details)) 100))))
       (point-max)

       ;; If our moving average is above or below nominal, shade the darker
       ;; area toward green or red. The further from nominal, the more intense
       ;; the color becomes, reaching full intensity at 1 work day.
       (jobhours-make-text-properties
        (alist-get 'logged-in details) pace))

      (buffer-string))))

(defvar jobhours-string "")
(put 'my-jobhours-string 'risky-local-variable t)

(defun jobhours-update-string ()
  (setq my-jobhours-string (get-jobhours-string)))

(defun jobhours-setup-modeline ()
  (run-at-time 0 300 #'jobhours-update-string)
  (push " " (default-value 'mode-line-format))
  (push '(:eval jobhours-string) (default-value 'mode-line-format)))

(provide 'jobhours)
