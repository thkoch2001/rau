.PHONY: test test-interactiv river

test:
	emacs --batch -Q -L . -L test -l ert \
	  -l reka-tests.el \
	  -l reka-tests-functional.el \
	  -l ewc-tags-tests.el \
	  -f ert-run-tests-batch-and-exit

test-interactiv:
	emacs -nw -Q -L . -L test -l ert \
	  -l reka-tests.el \
	  -l reka-tests-functional.el \
	  -l ewc-tags-tests.el \
	  --eval="(ert t)" -f delete-other-windows

river:
# opens a window in the parent X11/wayland session
# number of outputs to create
	export WLR_WL_OUTPUTS=1 && \
	river -log-level debug -no-xwayland -c "env|grep WAYLAND && sleep infinity"

# connect a new emacs instance to the above river process
# TODO: look into
# https://docs.gtk.org/glib/running.html
# export G_MESSAGES_DEBUG=all
test-emacs:
	export WAYLAND_DISPLAY=wayland-2 && \
	emacs -Q -L . -L test -l manualtestinit.el
