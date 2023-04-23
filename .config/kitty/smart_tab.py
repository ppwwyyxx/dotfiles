# This plugin is good for a workflow using kitty for local sessions but tmux for remote,
# with the same set of keybindins to manipulate kitty windows/tabs & tmux panes/windows.
# Inspired by https://github.com/yurikhan/kitty-smart-tab/blob/master/smart_tab.py
# but with more functionalities.
import logging
import kitty.key_encoding as ke
from kitty.options.utils import parse_key_action

logger = logging.getLogger(__name__)
logger.propagate = False
if not logger.hasHandlers():
    logger.setLevel(level=logging.INFO)
    logger.addHandler(logging.FileHandler("/tmp/smart_tab.log"))


def main(args):
    pass


def last_line_of_window(window):
    return window.as_text().split("\n")[-1]


def is_tmux_window(window):
    last = last_line_of_window(window)
    if "" in last or "No next window" in last:  # Match my custom tmux bar
        logger.info("Last line of window: " + last)
        return True
    return False


def send_key_to_window(window, key):
    keys = key.split(">")
    sent = b''
    for key in keys:
        mods, key = ke.parse_shortcut(key)
        shift, alt, ctrl, super, hyper, meta, caps_lock, num_lock = (
            bool(mods & bit) for bit in (
                ke.SHIFT, ke.ALT, ke.CTRL, ke.SUPER,
                ke.HYPER, ke.META, ke.CAPS_LOCK, ke.NUM_LOCK))

        for act in [ke.PRESS]:
            key_event = ke.KeyEvent(
                type=act, mods=mods, key=key,
                shift=shift, alt=alt, ctrl=ctrl, super=super,
                hyper=hyper, meta=meta, caps_lock=caps_lock, num_lock=num_lock)
            window_system_event = key_event.as_window_system_event()
            sequence = window.encoded_key(window_system_event)
            sent += sequence
            window.write_to_child(sequence)
    logger.info(f"Sent {repr(sent)} with length {len(sent)} to child.")
    # import rpdb
    # debugger = rpdb.Rpdb(port=12345)
    # debugger.set_trace()


def handle_result(args, answer, target_window_id, boss):
    action = args[1]

    tm = boss.active_tab_manager
    if tm is None:
        return

    def detach_window():
        if len(tm.active_tab) > 1:
            boss.dispatch_action(parse_key_action("detach_window new-tab"))

    if action in ["next_tab", "previous_tab"]:
        # Dispatch to kitty if any tabs exist.
        if len(tm.tabs) > 1:
            boss.dispatch_action(parse_key_action(action))
        else:
            # Otherwise, dispatch args[2] to window.
            w = boss.window_id_map.get(target_window_id)
            send_key_to_window(w, args[2])
    elif action in ["new_tab", "vsplit", "hsplit",
                    "neighboring_window", "resize_window",
                    "detach_window"]:
        w = boss.window_id_map.get(target_window_id)
        has_tmux = is_tmux_window(w)
        if has_tmux:
            # Detach tmux window.
            detach_window()
            # If tmux is running, dispatch to tmux.
            send_key_to_window(w, args[-1])
        else:
            if action.endswith("split"):
                full_action = f"launch --location={action} --cwd=current"
            else:
                full_action = " ".join(args[1:-1])
            boss.dispatch_action(parse_key_action(full_action))
    elif action in ["scroll_up"]:
        w = boss.window_id_map.get(target_window_id)
        has_tmux = is_tmux_window(w)
        if has_tmux:
            detach_window()
            send_key_to_window(w, "ctrl+q>K")
        else:
            boss.dispatch_action(parse_key_action("show_scrollback"))
            # Active window is now different
            send_key_to_window(tm.active_window, "u")


handle_result.no_ui = True
