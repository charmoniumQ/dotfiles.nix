;;; ...  -*- lexical-binding: t -*-
;;; packages.el --- Summary:
;;; Commentary:

;;; Code:

(package! vlf)

(package! trashed)

(package! rainbow-delimiters)

; Now included in data
;; (package! csv-mode)

(package! elmacro)

(package! helm-make)

(package! direnv)

(package! string-inflection)

(package! systemd)

(package! snakemake-mode)

; Org stuff
(package! org-edna)
(package! org-ql)
(package! helm-org-ql)
(package! org-super-agenda)
(package! org-caldav)
(package! org-super-agenda)

; Ugh. AI stuff
(package! gptel :recipe (:nonrecursive t))

(provide 'packages)
;;; packages.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
