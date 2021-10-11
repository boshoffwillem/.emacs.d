;;; xunit-mode.el --- Minor mode to speed up unit testing with Xunit.

;; Author: Willem Boshoff
;;
;; Keywords: xunit csharp testing unit

;;; Code:

(defun xunit-go-to-test-attributes ()
  (search-backward-regexp "\([\[]\)\(\(Fact\)\|\(Theory\)\|\(InlineData\)\)")
  )

(defun xunit-toggle-deferred ()
  (interactive)
  (xunit-go-to-test-attributes)
  )

(global-set-key (kbd "C-c C-x td") 'xunit-toggle-deferred)

(provide 'xunit-mode)

[Fact]
public class void Test1()
{
}

[Theory]
[InlineData()]
[InlineData()]
public class void Test1()
{
}
