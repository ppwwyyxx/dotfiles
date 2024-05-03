

function clientListToSwitch() {
	var curScreen = workspace.activeScreen;
	var curDesktop = workspace.currentDesktop;
	var clients = workspace.windowList();

	var res = new Array();
	for (var i=0; i<clients.length; i++) {
    // API reference: https://github.com/KDE/kwin//blob/2103eb8d162340d25ee52c04951e088b1cdfd700/src/window.h#L83
    if (!clients[i].normalWindow)
        continue;
		if (clients[i].output != curScreen)
      continue;
		if (clients[i].desktops.length > 0 && clients[i].desktops.indexOf(curDesktop) == -1)
			continue;
		res.push(clients[i]);
	}
	return res;
}

function switchClientRel(rel) {
	var curClient = workspace.activeWindow;
	var clients = clientListToSwitch();
  /*
   *for (var i = 0; i < clients.length; i++)
   *  print(clients[i].caption);
   */
	var curIdx = clients.indexOf(curClient);
	var nextClient = clients[(curIdx + rel + clients.length) % clients.length];
	workspace.activeWindow = nextClient;
}

//clientListToSwitch();
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
        workspace.slotSwitchToNextScreen();
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

