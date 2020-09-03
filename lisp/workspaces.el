;; Set up something to manage workspaces to keep projects separate.
;; Should have completion for:
;;    workspace specific buffer-list (leader-b)
;;    all workspaces buffer-list (leader-B)
;;    workspace-list (C-u leader-b)
;; Using Ivy for completion:
;;    should be able to rename workspaces in workspace-list completion
;;    move buffers to different workspace in buffer-lists completion
;; Auto add workspace when opening file in a project directory (use project.el?)
;; Auto-switch between workspaces based on project directory
;; If opening file not in a project directory keep in current workspace
;;    For buffers containing REPL's or what not
;; Manually add new workspace
;; Possibility to move files
;; Frames should be able to be in different workspaces
;;    Frame local vars?
;; Default workspace for buffer's that slipped by getting assigned to a workspace to fallback to
;; Alist of buffers:
;;    Top level workspace's associated with a list of buffers
;;    Moving to a different workspace means moving out of current workspace buffer list to separete one
