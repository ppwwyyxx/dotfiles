
function switchClientRel(rel) {
  var curClient = workspace.activeWindow;
  var clients = workspace.windowList();
  var curIdx = clients.indexOf(curClient);
  if (curIdx == -1) {
    console.log("Could not find current client?", curClient.caption);
    return;
  }

  /*
   *for (var i=0; i < clients.length; i++) {
   *  console.log(JSON.stringify(clients[i], null, 4));
   *}
   */

  // Find next client on the current screen & desktop to show.
  var curScreen = workspace.activeScreen;
  var curDesktop = workspace.currentDesktop;
  for (var i=0; i < clients.length; i++) {
    var rel_index = (rel == 1) ? (rel + i) : (rel - i);
    var nextClient = clients[(curIdx + rel_index + clients.length) % clients.length];

    // Skip some weird windows. They can cause crash in set activeWindow.
    if (!nextClient.normalWindow || nextClient.skipTaskbar)
      continue;
    if (nextClient.output != curScreen) // not on current screen
      continue;
    var desktops = nextClient.desktops;
    // If desktops.length == 0, it means window is on all desktops.
    if (desktops.length > 0 && nextClient.desktops.indexOf(curDesktop) == -1)
      continue;
    if (nextClient.width <= 1 || nextClient.height <= 1)
      // Some apps (zotero) start a tiny 1x1 off-screen window.
      continue;
    if (!nextClient.managed)
      continue;
    //print("Switching to ... ", nextClient.caption, nextClient, nextClient.caption == null);
    workspace.activeWindow = nextClient;
    return;
  }
}

registerShortcut("SwitchToNextClient", "(custom) Switch to Next Client",
  "", function() { switchClientRel(1); });
registerShortcut("SwitchToPrevClient", "(custom) Switch to Previous Client",
  "", function() { switchClientRel(-1); });



function windowToNextScreen() {
    var oldClient = workspace.activeWindow;
    workspace.slotWindowToNextScreen();
    var newClient = workspace.activeWindow;
    if (oldClient == newClient) {
        workspace.slotWindowRaise();
        // The window manager is configured to consider the screen with the mouse on it as active one.
        // Therefore it is not possible to switch to a screen explicitly.
        // workspace.slotSwitchToNextScreen();

        // TODO: move mouse
    }
};

registerShortcut("MoveToNextScreen", "(custom) Move Window to Next Screen", "", windowToNextScreen);


var fixWechat = function() {
    var clients = workspace.windowList();
    for (var i=0; i<clients.length; i++) {
        if ((clients[i].caption.indexOf("WeChat") == 0 || clients[i].caption.indexOf("微信") == 0)
            && clients[i].desktops.indexOf(workspace.currentDesktop) == -1) {
            // minimize wechat if it's not on current desktop
            // https://github.com/vufa/deepin-wine-wechat-arch/issues/201#issuecomment-1188833005
            clients[i].minimized = true;
        }
    }
};
workspace.currentDesktopChanged.connect(fixWechat);

