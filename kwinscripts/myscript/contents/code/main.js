

function clientListToSwitch() {
	var curScreen = workspace.activeScreen;
	var curDesktop = workspace.currentDesktop;

	var clients = workspace.clientList();
	var res = new Array();
	for (var i=0; i<clients.length; i++) {
		if (clients[i].screen != curScreen)
			continue;
		if (clients[i].desktop > 0 && clients[i].desktop != curDesktop)
			continue;
		if (clients[i].skipSwitcher || clients[i].skipPager || clients[i].skipTaskbaer || clients[i].specialWindow)
			continue;
		res.push(clients[i]);
	}
	return res;
}

function switchClientRel(rel) {
	var curClient = workspace.activeClient;
	var clients = clientListToSwitch();
	var curIdx = clients.indexOf(curClient);
	var nextClient = clients[(curIdx + rel + clients.length) % clients.length];
	workspace.activeClient = nextClient;
}

//clientListToSwitch();
registerShortcut("SwitchToNextClient", "(custom) Switch to Next Client",
	"", function() { switchClientRel(1); });
registerShortcut("SwitchToPrevClient", "(custom) Switch to Previous Client",
	"", function() { switchClientRel(-1); });



function windowToNextScreen() {
    var oldClient = workspace.activeClient;
    workspace.slotWindowToNextScreen();
    var newClient = workspace.activeClient;
    if (oldClient == newClient) {
        workspace.slotWindowRaise();
        workspace.slotSwitchToNextScreen();
        // TODO: move mouse
    }
};

registerShortcut("MoveToNextScreen", "(custom) Move Window to Next Screen", "", windowToNextScreen);
