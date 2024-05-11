// https://github.com/wsdfhjxc/kwin-scripts/blob/master/virtual-desktops-only-on-primary/contents/code/main.js
function bind(window) {
    window._previousScreen = window.output; // set a custom attribute

    var callback = function() {
      update(window);
    };

    if (window.normalWindow) {
      window.outputChanged.connect(callback);
      // window.desktopsChanged.connect(callback); should be unnecessary
      print("Window " + window.caption + " has been connected");
    }
}

function update(window) {
    var window = window || this;

    if (!window.normalWindow) {
        return;
    }

    var primaryScreen = workspace.screens[0];
    var currentScreen = window.output;
    var previousScreen = window._previousScreen;
    window._previousScreen = currentScreen;

    if (currentScreen != primaryScreen) { // window moved to non-primary
        window.onAllDesktops = true;
        print("Window " + window.caption + " has been pinned");
    } else if (previousScreen != primaryScreen) {  // window moved from non-primary to primary
        window.onAllDesktops = false;
        window.desktops = Array.from([workspace.currentDesktop]);
        print("Window " + window.caption+ " has been unpinned");
    }
}

function bindUpdate(window) {
    bind(window);
    update(window);
}

function main() {
    workspace.windowList().forEach(bind);
    workspace.windowList().forEach(update);
    workspace.windowAdded.connect(bindUpdate);
}

main();

