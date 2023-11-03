

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
function logWindows(windows) {
  for (k of windows) {
    console.log("Window", k.title(), "from App=", k.app().name(), ",visible=", k.isVisible());
  }
}

// string -> Rectangle
const windowRestoreFrame = {};
// Toggle maximize.
Key.on('up', ['alt'], () => {
  const window = getCurrWindow();
  const hash = window.hash();
  // Toggle maximize.
  if (isMaximized(window)) {
    if (windowRestoreFrame[hash]) {
      window.setFrame(windowRestoreFrame[hash])
    }
  } else {
    windowRestoreFrame[hash]= window.frame();
    Window.focused().maximize();
  }
});
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
Key.on('f9', ['cmd'], () => {
  const window = getCurrWindow();
  if (window !== undefined) 
    window.minimize();
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

