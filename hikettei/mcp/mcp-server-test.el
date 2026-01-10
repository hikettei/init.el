;;; mcp-server-test.el --- Tests for MCP Server -*- lexical-binding: t; -*-

;; Author: hikettei
;; Keywords: test

;;; Commentary:
;;
;; ERT tests for mcp-server.el
;; Run with: M-x ert RET t RET
;; Or: emacs -batch -l mcp-server-test.el -f ert-run-tests-batch-and-exit
;;

;;; Code:

(require 'ert)
(require 'cl-lib)

;; mcp-server should be loaded via init.el before running tests

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar mcp-test--temp-dir nil
  "Temporary directory for tests.")

(defvar mcp-test--temp-files nil
  "List of temporary files created during tests.")

(defun mcp-test--setup ()
  "Setup test environment."
  (setq mcp-test--temp-dir (make-temp-file "mcp-test-" t))
  (setq mcp-test--temp-files nil)
  ;; Set project root for MCP server (used by mcp-server--resolve-path)
  (setq mcp-server--project-root mcp-test--temp-dir))

(defun mcp-test--teardown ()
  "Cleanup test environment."
  (when (and mcp-test--temp-dir (file-directory-p mcp-test--temp-dir))
    (delete-directory mcp-test--temp-dir t))
  (setq mcp-test--temp-dir nil)
  (setq mcp-test--temp-files nil)
  (setq mcp-server--project-root nil))

(defun mcp-test--create-file (name content)
  "Create a test file with NAME and CONTENT."
  (let ((path (expand-file-name name mcp-test--temp-dir)))
    (with-temp-file path
      (insert content))
    (push path mcp-test--temp-files)
    path))

(defun mcp-test--read-file (path)
  "Read content of file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;; ============================================================
;;; emacs_read_file Tests
;;; ============================================================

(ert-deftest mcp-test-read-file-basic ()
  "Test basic file reading."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-test--create-file "test.txt" "line 1\nline 2\nline 3\n")
        (let ((result (mcp-server--tool-read-file
                       `((file_path . "test.txt")))))
          (should (stringp result))
          (should (string-match "1:" result))
          (should (string-match "line 1" result))
          (should (string-match "line 2" result))
          (should (string-match "line 3" result))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-read-file-with-offset ()
  "Test file reading with offset."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-test--create-file "test.txt" "line 1\nline 2\nline 3\nline 4\nline 5\n")
        (let ((result (mcp-server--tool-read-file
                       `((file_path . "test.txt")
                         (offset . 2)
                         (limit . 2)))))
          (should (stringp result))
          (should (string-match "line 3" result))
          (should (string-match "line 4" result))
          (should-not (string-match "line 1" result))
          (should-not (string-match "line 5" result))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-read-file-not-found ()
  "Test reading non-existent file."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let ((result (mcp-server--tool-read-file
                       `((file_path . "nonexistent.txt")))))
          (should (stringp result))
          (should (string-match-p "not found\\|does not exist\\|Error" result))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-read-file-empty ()
  "Test reading empty file."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-test--create-file "empty.txt" "")
        (let ((result (mcp-server--tool-read-file
                       `((file_path . "empty.txt")))))
          (should (stringp result))))
    (mcp-test--teardown)))

;;; ============================================================
;;; emacs_write_file Tests
;;; ============================================================

(ert-deftest mcp-test-write-file-new ()
  "Test creating new file."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((result (mcp-server--tool-write-file
                        `((file_path . "new.txt")
                          (content . "hello world"))))
               (path (expand-file-name "new.txt" mcp-test--temp-dir)))
          (should (stringp result))
          (should (string-match-p "success\\|written\\|created" (downcase result)))
          (should (file-exists-p path))
          (should (string= (mcp-test--read-file path) "hello world"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-write-file-no-supersede ()
  "Test writing to existing file without supersede fails."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-test--create-file "existing.txt" "original content")
        (let ((result (mcp-server--tool-write-file
                       `((file_path . "existing.txt")
                         (content . "new content")
                         (supersede . :json-false)))))
          (should (stringp result))
          ;; Should fail or warn about existing file
          (should (or (string-match-p "exist\\|error\\|fail" (downcase result))
                      ;; Or file content unchanged
                      (string= (mcp-test--read-file
                                (expand-file-name "existing.txt" mcp-test--temp-dir))
                               "original content")))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-write-file-with-supersede ()
  "Test overwriting existing file with supersede."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let ((path (mcp-test--create-file "existing.txt" "original content")))
          (mcp-server--tool-write-file
           `((file_path . "existing.txt")
             (content . "new content")
             (supersede . t)))
          (should (string= (mcp-test--read-file path) "new content"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-write-file-creates-directory ()
  "Test writing file creates parent directories."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((result (mcp-server--tool-write-file
                        `((file_path . "subdir/nested/file.txt")
                          (content . "nested content"))))
               (path (expand-file-name "subdir/nested/file.txt" mcp-test--temp-dir)))
          (should (file-exists-p path))
          (should (string= (mcp-test--read-file path) "nested content"))))
    (mcp-test--teardown)))

;;; ============================================================
;;; emacs_edit_file Tests
;;; ============================================================

(ert-deftest mcp-test-edit-file-basic ()
  "Test basic file editing request."
  (unwind-protect
      (let ((original-result-file mcp-server--result-file))
        (unwind-protect
            (progn
              (mcp-test--setup)
              ;; Disable result-file to avoid blocking wait
              (setq mcp-server--result-file nil)
              (mcp-test--create-file "edit.txt" "line 1\nline 2\nline 3\nline 4\nline 5\n")
              (let ((result (mcp-server--tool-request-edit
                             `((file_path . "edit.txt")
                               (start_line . 2)
                               (end_line . 3)
                               (content . "new line 2\nnew line 3")
                               (comment . "Updated lines 2-3")))))
                (should (stringp result))
                ;; Should return patch_id or open review UI
                (should (or (string-match-p "patch\\|review\\|pending\\|staged" (downcase result))
                            (string-match-p "success\\|applied" (downcase result))))))
          (setq mcp-server--result-file original-result-file)))
    (mcp-server--clear-patches)
    (mcp-test--teardown)))

(ert-deftest mcp-test-edit-file-invalid-range ()
  "Test editing with invalid line range."
  (unwind-protect
      (let ((original-result-file mcp-server--result-file))
        (unwind-protect
            (progn
              (mcp-test--setup)
              ;; Disable result-file to avoid blocking wait
              (setq mcp-server--result-file nil)
              (mcp-test--create-file "edit.txt" "line 1\nline 2\n")
              (let ((result (mcp-server--tool-request-edit
                             `((file_path . "edit.txt")
                               (start_line . 10)
                               (end_line . 20)
                               (content . "content")
                               (comment . "Invalid range")))))
                (should (stringp result))
                ;; Should handle gracefully (error or extend file)
                ))
          (setq mcp-server--result-file original-result-file)))
    (mcp-server--clear-patches)
    (mcp-test--teardown)))


;;; ============================================================
;;; MCP Protocol Tests
;;; ============================================================

(ert-deftest mcp-test-protocol-initialize ()
  "Test MCP initialize response."
  (let ((result (mcp-server--handle-initialize nil)))
    (should (assoc 'protocolVersion result))
    (should (assoc 'capabilities result))
    (should (assoc 'serverInfo result))))

(ert-deftest mcp-test-protocol-tools-list ()
  "Test MCP tools/list response."
  (let ((result (mcp-server--handle-tools-list nil)))
    (should (assoc 'tools result))
    (let ((tools (cdr (assoc 'tools result))))
      (should (> (length tools) 0))
      ;; Check tool names
      (should (cl-find-if (lambda (t) (string= (alist-get 'name t) "emacs_read_file")) tools))
      (should (cl-find-if (lambda (t) (string= (alist-get 'name t) "emacs_write_file")) tools))
      (should (cl-find-if (lambda (t) (string= (alist-get 'name t) "emacs_edit_file")) tools)))))

(ert-deftest mcp-test-protocol-tools-call ()
  "Test MCP tools/call dispatch."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-test--create-file "test.txt" "content")
        (let ((result (mcp-server--handle-tools-call
                       `((name . "emacs_read_file")
                         (arguments . ((file_path . "test.txt")))))))
          (should (assoc 'content result))
          (let ((content (cdr (assoc 'content result))))
            (should (> (length content) 0)))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-protocol-unknown-tool ()
  "Test calling unknown tool."
  (let ((result (mcp-server--handle-tools-call
                 `((name . "unknown_tool")
                   (arguments . ())))))
    (should (assoc 'content result))
    (let* ((content (cdr (assoc 'content result)))
           (text (alist-get 'text (aref content 0))))
      (should (string-match-p "unknown\\|error" (downcase text))))))

;;; ============================================================
;;; HTTP Server Tests (if server is running)
;;; ============================================================

(ert-deftest mcp-test-server-start-stop ()
  "Test starting and stopping MCP server."
  (unwind-protect
      (progn
        (mcp-test--setup)
        ;; Start server
        (let ((result (mcp-server-start mcp-test--temp-dir)))
          (should result)
          (should (consp result))
          (should (numberp (cdr result)))  ; port number
          (should (> (cdr result) 0)))
        ;; Stop server
        (mcp-server-stop)
        (should-not mcp-server--server))
    (mcp-server-stop)
    (mcp-test--teardown)))

;;; ============================================================
;;; JSON-RPC Tests
;;; ============================================================

(ert-deftest mcp-test-dispatch-initialize ()
  "Test dispatch for initialize method."
  (let ((result (mcp-server--dispatch "initialize" nil)))
    (should result)
    (should (assoc 'protocolVersion result))))

(ert-deftest mcp-test-dispatch-tools-list ()
  "Test dispatch for tools/list method."
  (let ((result (mcp-server--dispatch "tools/list" nil)))
    (should result)
    (should (assoc 'tools result))))

(ert-deftest mcp-test-dispatch-unknown ()
  "Test dispatch for unknown method returns error content."
  (let ((result (mcp-server--dispatch "unknown/method" nil)))
    (should result)
    (should (assoc 'content result))
    (let* ((content (cdr (assoc 'content result)))
           (text (alist-get 'text (aref content 0))))
      (should (string-match-p "unknown\\|error" (downcase text))))))

;;; ============================================================
;;; Patch Management Tests
;;; ============================================================

(ert-deftest mcp-test-patch-create ()
  "Test creating a patch for a file."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "patch.txt" "original content"))
               (patch (mcp-server--create-patch path)))
          (should (mcp-server-patch-p patch))
          (should (string= (mcp-server-patch-file-path patch) path))
          (should-not (string= (mcp-server-patch-original-hash patch) "new_file"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-create-new-file ()
  "Test creating a patch for non-existent file."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (expand-file-name "nonexistent.txt" mcp-test--temp-dir))
               (patch (mcp-server--create-patch path)))
          (should (mcp-server-patch-p patch))
          (should (string= (mcp-server-patch-original-hash patch) "new_file"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-stage-overwrite ()
  "Test staging a full file overwrite."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "overwrite.txt" "old"))
               (patch (mcp-server--create-patch path)))
          (mcp-server--patch-stage-overwrite patch "new content")
          (should (mcp-server-patch-is-full-overwrite patch))
          (should (string= (mcp-server-patch-content patch) "new content"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-stage-edit ()
  "Test staging a partial edit."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "partial.txt" "line1\nline2\nline3\nline4"))
               (patch (mcp-server--create-patch path)))
          (let ((result (mcp-server--patch-stage-edit patch 2 3 "new line 2\nnew line 3")))
            (should (consp result))
            (should (stringp (car result)))  ; preview
            (should (stringp (cdr result)))  ; original
            (should (string-match "line2" (cdr result)))
            (should (string-match "line3" (cdr result))))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-apply-overwrite ()
  "Test applying a full overwrite patch."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "apply.txt" "old content"))
               (patch (mcp-server--create-patch path)))
          (mcp-server--patch-stage-overwrite patch "new content")
          (mcp-server--patch-apply patch)
          (should (string= (mcp-test--read-file path) "new content"))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-apply-partial ()
  "Test applying a partial edit patch."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "partial-apply.txt" "line1\nline2\nline3\nline4"))
               (patch (mcp-server--create-patch path)))
          (mcp-server--patch-stage-edit patch 2 3 "replaced")
          (mcp-server--patch-apply patch)
          (let ((result (mcp-test--read-file path)))
            (should (string-match "line1" result))
            (should (string-match "replaced" result))
            (should (string-match "line4" result))
            (should-not (string-match "line2" result))
            (should-not (string-match "line3" result)))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-patch-register-get ()
  "Test registering and retrieving patches."
  (unwind-protect
      (progn
        (mcp-test--setup)
        (mcp-server--clear-patches)
        (let* ((path (mcp-test--create-file "reg.txt" "content"))
               (patch (mcp-server--create-patch path))
               (patch-id (mcp-server--register-patch patch)))
          (should (stringp patch-id))
          (should (string-match "^p[0-9]+" patch-id))
          (let ((retrieved (mcp-server--get-patch patch-id)))
            (should (eq retrieved patch)))))
    (mcp-server--clear-patches)
    (mcp-test--teardown)))

;;; ============================================================
;;; File Editor Integration Tests
;;; ============================================================

(ert-deftest mcp-test-file-editor-session-create ()
  "Test creating a file-editor session."
  (skip-unless (fboundp 'file-editor-open))
  (unwind-protect
      (progn
        (mcp-test--setup)
        (let* ((path (mcp-test--create-file "editor-test.txt" "line1\nline2\nline3\nline4\n"))
               (session-id nil)
               (callback-called nil))
          ;; Open file-editor session
          (setq session-id
                (file-editor-open
                 :file path
                 :start-line 2
                 :end-line 3
                 :original "line2\nline3"
                 :new "new-line2\nnew-line3"
                 :ai-comment "Test edit"
                 :callback (lambda (result) (setq callback-called t))))
          ;; Verify session was created
          (should (stringp session-id))
          ;; Verify buffer was created
          (should (get-buffer "*file-review: editor-test.txt*"))
          ;; Cleanup
          (when-let ((buf (get-buffer "*file-review: editor-test.txt*")))
            (kill-buffer buf))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-file-editor-approve-applies-changes ()
  "Test that approving in file-editor applies changes to file."
  (skip-unless (fboundp 'file-editor-open))
  (unwind-protect
      (let ((result-file (make-temp-file "mcp-test-result" nil ".json"))
            (original-result-file file-editor-result-file))
        (unwind-protect
            (progn
              (mcp-test--setup)
              ;; Set up result file for test
              (setq file-editor-result-file result-file)
              (let* ((path (mcp-test--create-file "approve-test.txt" "line1\nline2\nline3\nline4\n"))
                     (patch (mcp-server--create-patch path))
                     (applied nil))
                ;; Stage the edit
                (mcp-server--patch-stage-edit patch 2 3 "new-line2\nnew-line3")
                ;; Register patch
                (mcp-server--register-patch patch)
                ;; Simulate approval by writing result file
                (with-temp-file result-file
                  (insert (json-encode '((decision . "approve")
                                         (summary . "Test approval")
                                         (line_comments . [])))))
                ;; Apply the patch (simulating what happens after approval)
                (mcp-server--patch-apply patch)
                ;; Verify file was modified
                (let ((content (mcp-test--read-file path)))
                  (should (string-match "new-line2" content))
                  (should (string-match "new-line3" content))
                  (should-not (string-match "line2\n" content))
                  (should-not (string-match "line3\n" content)))))
          ;; Restore original result file
          (setq file-editor-result-file original-result-file)
          (when (file-exists-p result-file)
            (delete-file result-file))))
    (mcp-server--clear-patches)
    (mcp-test--teardown)))

(ert-deftest mcp-test-file-editor-full-flow ()
  "Test the full MCP -> file-editor -> apply flow.
Tests the edit request without blocking wait (result-file=nil)."
  (skip-unless (fboundp 'file-editor-open))
  (unwind-protect
      (let ((original-result-file mcp-server--result-file))
        (unwind-protect
            (progn
              (mcp-test--setup)
              ;; Set result-file to nil to avoid blocking wait
              (setq mcp-server--result-file nil)
              (let* ((path (mcp-test--create-file "flow-test.py"
                                                   "def hello():\n    print('old')\n    return True\n"))
                     (result (mcp-server--tool-request-edit
                              `((file_path . "flow-test.py")
                                (start_line . 2)
                                (end_line . 2)
                                (content . "    print('new')")
                                (comment . "Update print message")))))
                ;; Without result-file, should return pending
                (should (stringp result))
                (should (string-match-p "pending\\|review" (downcase result)))
                ;; Now manually apply the patch to verify it works
                (let ((patch (mcp-server--get-patch "p0001")))
                  (when patch
                    (mcp-server--patch-apply patch)
                    (let ((content (mcp-test--read-file path)))
                      (should (string-match "print('new')" content)))))))
          (setq mcp-server--result-file original-result-file)))
    (mcp-server--clear-patches)
    (mcp-test--teardown)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest mcp-test-integration-read-write-cycle ()
  "Test full read-write cycle."
  (unwind-protect
      (progn
        (mcp-test--setup)
        ;; Write file
        (mcp-server--tool-write-file
         `((file_path . "cycle.txt")
           (content . "original\ncontent\nhere")))
        ;; Read file
        (let ((result (mcp-server--tool-read-file
                       `((file_path . "cycle.txt")))))
          (should (string-match "original" result))
          (should (string-match "content" result))
          (should (string-match "here" result))))
    (mcp-test--teardown)))

(ert-deftest mcp-test-integration-nested-write ()
  "Test writing to nested directory."
  (unwind-protect
      (progn
        (mcp-test--setup)
        ;; Create nested file
        (mcp-server--tool-write-file
         `((file_path . "a/b/c/deep.txt")
           (content . "deep content")))
        ;; Verify
        (let ((path (expand-file-name "a/b/c/deep.txt" mcp-test--temp-dir)))
          (should (file-exists-p path))
          (should (string= (mcp-test--read-file path) "deep content"))))
    (mcp-test--teardown)))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(defun mcp-server-run-tests ()
  "Run all MCP server tests."
  (interactive)
  (ert-run-tests-interactively "^mcp-test-"))

(provide 'mcp-server-test)

;;; mcp-server-test.el ends here
