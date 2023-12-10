const YABAI_PATH = '/opt/homebrew/bin/yabai';

// f[[ modal
function showTitleModal(text, duration, icon) {
    const modal = new Modal();
    modal.text = text;
    modal.duration = duration;
    if (icon) { modal.icon = icon; }
    showAt(modal, Screen.main(), 2, 1 + 1 / 3);
}

function showAt(modal, screen, widthDiv, heightDiv) {
    const {height, width, x, y} = modal.frame();
    const sf = screen.visibleFrame();
    modal.origin = {
        x: sf.x + (sf.width / widthDiv - width / 2),
        y: sf.y + (sf.height / heightDiv - height / 2),
    };
    modal.show();
}
// f]]

// f[[ Screen functions:
function get_next_screen(screen) {
  const all_screens = Screen.all();
  const idx = all_screens.indexOf(screen);
  return all_screens[(idx + 1) % all_screens.length];
}
function get_screen_at(x, y) {
  for (s of Screen.all()) {
    const frame = s.flippedFrame();
    if (frame.x <= x && frame.y <= y && 
      x <= frame.x + frame.width && y <= frame.y + frame.height) {
      return s;
    }
  }
  console.error("Cannot find screen at ", x, y)
}
// f]]


// f[[ Windows:
function isMaximized(window) {
  const screen = window.screen().visibleFrame();
  const wSize = window.size();
  return Math.abs(wSize.width - screen.width) <= 2 && Math.abs(wSize.height - screen.height) <= 2;
}
function getCurrWindow() {
  const w = Window.focused();
  if (w !== undefined) {
      return w;
  }
  // TODO sometime this is undefined
  return App.focused().mainWindow();
}
function logWindows(windows) { // Print details about a list of windows
  for (k of windows) {
    console.log("Window", k.title(), "from App=", k.app().name(), ",visible=", k.isVisible());
  }
}

function move_mouse_to_window(window) {
  const frame = window.frame();
  const x = frame.x + frame.width / 2, y = frame.y + frame.height / 2;
  Mouse.move({x: x, y:y});
}

// string -> Rectangle
const _windowRestoreFrame = {};
function _save_rel_window_frame(window) {
  const screen = window.screen().flippedVisibleFrame();
  const frame = window.frame();
  const dx = (frame.x - screen.x) / screen.width,
        dy = (frame.y - screen.y) / screen.height,
        width = frame.width / screen.width,
        height= frame.height / screen.height;
  _windowRestoreFrame[window.hash()] = {x:dx, y:dy, width:width, height:height};
}
function _maybe_restore_rel_window_frame(window) {
  const hash = window.hash();
  const rect = _windowRestoreFrame[hash]
  if (rect) {
    const screen = window.screen().flippedVisibleFrame();
    const width = rect.width * screen.width,
          height = rect.height * screen.height,
          x = rect.x * screen.width + screen.x,
          y = rect.y * screen.height + screen.y;
    window.setFrame({x:x, y:y, width:width, height:height});
  }
}

Key.on('up', ['alt'], () => {   // Toggle maximize.
  const window = getCurrWindow();
  const hash = window.hash();
  // Toggle maximize.
  if (isMaximized(window)) {
    _maybe_restore_rel_window_frame(window);
  } else {
    _save_rel_window_frame(window);
    Window.focused().maximize();
  }
});

Key.on('o', ['alt'], () => {  // Move window to another screen.
  const window = getCurrWindow();
  const screen = window.screen();
  const next_screen = get_next_screen(screen).visibleFrame();
  if (screen == next_screen) return;
  const maximized = isMaximized(window);
  if (maximized) {
    window.setTopLeft({x: next_screen.x, y: next_screen.y}); // move
    window.maximize();
  } else {
    _save_rel_window_frame(window);
    window.setTopLeft({x: next_screen.x, y: next_screen.y}); // move
    _maybe_restore_rel_window_frame(window);
  }
  move_mouse_to_window(window);
});

Key.on('n', ['alt'], () => {  // Move mouse to another screen.
  const loc = Mouse.location();
  const screen = get_screen_at(loc.x, loc.y);
  const next_frame = get_next_screen(screen).flippedFrame();
  const frame = screen.flippedFrame();
  if (frame == next_frame) return;
  const rx = (loc.x - frame.x) / frame.width,
        ry = (loc.y - frame.y) / frame.height;
  const newPoint = {x: next_frame.x + rx * next_frame.width,
     y: next_frame.y + ry * next_frame.height};
  Mouse.move(newPoint);

  Task.run(YABAI_PATH, ['-m', 'display', '--focus', 'mouse']);
  // Window.at is broken!
  //const window = Window.at(newPoint);
  //if (window) { window.raise(); window.focus(); }
});

// Snap window to left/right edge.
Key.on('\'', ['alt'], () => {
  const window = getCurrWindow();
  const screen = window.screen().flippedVisibleFrame();
  screen.x = screen.width / 2;
  screen.width = screen.width - screen.x;
  window.setFrame(screen);
});
Key.on(';', ['alt'], () => {
  const window = getCurrWindow();
  const screen = window.screen().flippedVisibleFrame();
  screen.width = screen.width / 2;
  window.setFrame(screen);
});

Key.on('f9', ['cmd'], () => {  // Minimize
  const window = getCurrWindow();
  if (window !== undefined) window.minimize();
});

function cycleWindow(dir) {
  const window = getCurrWindow();
  if (!window) return;
  const others = window.screen().windows({"visible": true});
  //logWindows(others);
  if (others.length) {
    for (k = 0; k < others.length; ++k) {
      if (others[k].isEqual(window))
        break;
    }
    const next = (k + dir + others.length) % others.length;
    others[next].focus();
  }
}
Key.on('j', ['alt'], () => { cycleWindow(1); });
Key.on('k', ['alt'], () => { cycleWindow(-1); });

function moveToSpace(spaceId) { // spaceId: 1-based index
  const spaces = Space.all();
  if (spaceId > spaces.length) return;
  const space = spaces[spaceId - 1];
  const w = getCurrWindow();
  space.moveWindows([w]);
}
Key.on('1', ['alt', 'shift'], () => { moveToSpace(1); });
Key.on('2', ['alt', 'shift'], () => { moveToSpace(2); });
Key.on('3', ['alt', 'shift'], () => { moveToSpace(3); });
// How to switch screen

function resetWindowFocus(w) { // set focus by mouse
  if (w instanceof Window) {
    if (!w.isMinimised() && !w.isNormal()) {  // Popup/floats, don't reset focus due to them.
      console.log("Not Normal window:", w.title(), w.app().name());
      return;
    }
  }
  console.log("Normal window:", w.title(), w.app().name());
  const location = Mouse.location();
  const window = Window.at(location);
  // TODO: Could not get accessibility element at position
  if (window === undefined)
    return;
  console.log("Window under mouse", window.title());
  window.focus();
}
Event.on('windowDidClose', resetWindowFocus);
Event.on('spaceDidChange', resetWindowFocus);
Event.on('screensDidChange', resetWindowFocus);
// f]]

const phoenixApp = App.get('Phoenix');
showTitleModal('Phoenix (re)loaded!', 2, phoenixApp && phoenixApp.icon());

