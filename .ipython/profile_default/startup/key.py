# -*- coding: utf-8 -*-
from IPython import get_ipython
from prompt_toolkit.enums import DEFAULT_BUFFER
from prompt_toolkit.keys import Keys
from prompt_toolkit.filters import HasFocus, HasSelection, ViInsertMode, EmacsInsertMode
from prompt_toolkit.key_binding.bindings.named_commands import get_by_name


ip = get_ipython()
insert_mode = ViInsertMode() | EmacsInsertMode()

cursor_left = lambda ev: ev.current_buffer.cursor_left()
cursor_right = lambda ev: ev.current_buffer.cursor_right()

default_filters = HasFocus(DEFAULT_BUFFER) & ~HasSelection() & insert_mode

# Register the shortcut if IPython is using prompt_toolkit
if hasattr(ip, 'pt_cli'):
    registry = ip.pt_cli.application.key_bindings_registry
    registry.add_binding(Keys.ControlH, filter=default_filters)(cursor_left)
    registry.add_binding(Keys.ControlL, filter=default_filters)(cursor_right)
    registry.add_binding(Keys.ControlB,
            filter=default_filters)(get_by_name('backward-word'))

