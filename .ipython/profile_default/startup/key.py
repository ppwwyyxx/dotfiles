# Refs:
# https://ipython.readthedocs.io/en/stable/config/shortcuts/index.html#terminal-shortcuts-list
from IPython import get_ipython


def _get_default_filters():
    from prompt_toolkit.enums import DEFAULT_BUFFER
    from prompt_toolkit.filters import HasFocus, HasSelection, ViInsertMode, EmacsInsertMode
    insert_mode = ViInsertMode() | EmacsInsertMode()
    default_filters = HasFocus(DEFAULT_BUFFER) & ~HasSelection() & insert_mode
    return default_filters


def _configure_prompt_toolkit():
    from prompt_toolkit.keys import Keys
    import prompt_toolkit.key_binding.bindings.named_commands as named_commands

    default_filters = _get_default_filters()
    registry = get_ipython().pt_app.key_bindings

    # Left/right movement:
    cursor_left = lambda ev: ev.current_buffer.cursor_left()
    cursor_right = lambda ev: ev.current_buffer.cursor_right()
    registry.add(Keys.ControlL, filter=default_filters)(cursor_right)
    registry.add(Keys.ControlH, filter=default_filters)(cursor_left)
    # prompt_toolkit does not distinguish ctrl-h and backspace. 
    # https://github.com/prompt-toolkit/python-prompt-toolkit/blob/4432d6233fd8e0efba5920a9650e515f54a20300/src/prompt_toolkit/input/ansi_escape_sequences.py#L58-L64
    # This hack makes it work.
    from prompt_toolkit.input.ansi_escape_sequences import ANSI_SEQUENCES
    ANSI_SEQUENCES['\x7f'] = Keys.ControlF24
    registry.add(Keys.ControlF24, filter=default_filters)(named_commands.backward_delete_char)

    # Backward word:
    # Redefine word boundary for c-w
    # https://gist.github.com/fratajczak/64e32421a43d3b8194d0409ce300518a
    registry.add(Keys.ControlW,
                     filter=default_filters)(named_commands.backward_kill_word)
    # Ctrl-shift-w (probably specific to kitty)
    registry.add('escape', *'[119;6u', filter=default_filters)(named_commands.unix_word_rubout)
    registry.add(Keys.ControlB,
            filter=default_filters)(named_commands.backward_word)


    # Forward word, reimplement with include_current_position
    def my_kill_word(event) -> None:
        # https://github.com/prompt-toolkit/python-prompt-toolkit/blob/4432d6233fd8e0efba5920a9650e515f54a20300/src/prompt_toolkit/key_binding/bindings/named_commands.py#L370
        buff = event.current_buffer
        pos = buff.document.find_next_word_ending(count=event.arg,
                                                  include_current_position=True)
        if pos:
            deleted = buff.delete(count=pos)
            if event.is_repeat:
                deleted = event.app.clipboard.get_data().text + deleted
            event.app.clipboard.set_text(deleted)
    registry.add('escape', 'd', filter=default_filters)(my_kill_word)


# Register the shortcut if IPython is using prompt_toolkit
# if hasattr(get_ipython(), 'pt_app'):
_configure_prompt_toolkit()
del _configure_prompt_toolkit  # Do not pollute REPL namespace
