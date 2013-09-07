var BOSH_SERVICE = 'http://50.19.39.227/http-bind',
    TURN_CREDENTIALS_SERVICE = 'http://50.19.39.227/http-bind-turn',  
    DOMAIN = "erlocator.org",
    TURN_DOMAIN = "go.estos.de",
    CONFERENCEDOMAIN = 'conference.' + DOMAIN,
    ice_config = {iceServers: [{url: 'stun:stun.l.google.com:19302'}]},
    RTC = null,
    RTCPeerConnection = null,
    AUTOACCEPT = true,
    PRANSWER = false, // use either pranswer or autoaccept
    RAWLOGGING = true,
    MULTIPARTY = true,
    localStream = null,
    connection = null,
    myroomjid = null,
    roomjid = null,
    turn_service_connection = null,
    neighbors = [],
    handlers = [];

function getNeighbors(){
    $.getJSON(host+'geo/neighbors',"geonum="+client.geonum,function(response){
        /*
         {"neighbors":[{"geonum":62914559,"id":"1501","lat":"90","lon":"90"},{"geonum":62914558,"id":"1502","lat":"90","lon":"90"}]}
         */
         //clearMap();
         $.each(response.neighbors, function(index, neighbor) {
             if(neighbor.id != client.id){
                if(typeof(neighbor.name) == "undefined")neighbor.name = "Neighbor";
                if(typeof(neighbor.imageURL) == "undefined")neighbor.imageURL = "http://erlocator.org/geonum/demo/defaultsmall.gif";
                if(typeof(neighbor.profileURL) == "undefined")neighbor.profileURL = "http://erlocator.org";
                placeMarker(neighbor);
             }
         });
    });
}

function getNeighborInfo(userId){
    $.getJSON(host+'geo/get_neighbor',"id="+userId,function(response){
        /*
         {"geonum":62914559,"id":"1501","lat":"90","lon":"90"}}
         */
	 var neighbor = response;
             if(neighbor.id != client.id){
                if(typeof(neighbor.name) == "undefined")neighbor.name = "Neighbor";
                if(typeof(neighbor.imageURL) == "undefined")neighbor.imageURL = "http://erlocator.org/geonum/demo/defaultsmall.gif";
                if(typeof(neighbor.profileURL) == "undefined")neighbor.profileURL = "http://erlocator.org";
                placeMarker(neighbor);
		// Store neighbor with geonum, so we can do area cleanups later
		storeNeighbor(neighbor.geonum, neighbor.id);
             }
    });
}

function storeNeighbor(geonum, id) {
	neighbors["" + id] = geonum;
} 

function removeNeighbor(id) {
   delete neighbors[id];
}

function generateNeighbors(){
    //
    $.post(host+'geo/generate',{geonum:client.geonum,n: $( "#numneighbors div" ).slider( "value" )},
        function(obj) {
           getNeighbors();
        }
    );
}

function getBoundingBox(bbox_handler){
    $.getJSON(host+'geo/bbox',"geonum="+client.geonum,function(response){
        /*
         {"top_left":{"lat":90.0,"lon":89.9560546875},"bottom_right":{"lat":89.9560546875,"lon":90.0}, "tiles":[48932760, ....]}
         */
        //draw bounding box
        var tleft = response.bbox_3x3.top_left;
        var bright = response.bbox_3x3.bottom_right;
        var bound = new google.maps.LatLngBounds();
        bound.extend(new google.maps.LatLng(tleft.lat,tleft.lon));
        bound.extend(new google.maps.LatLng(bright.lat,bright.lon));

        ne = bound.getNorthEast();
        sw = bound.getSouthWest();
        se = new google.maps.LatLng(sw.lat(),ne.lng());
        nw = new google.maps.LatLng(ne.lat(),sw.lng());
        map_c = new google.maps.LatLng(ne.lat(),-180);
        map_ne = new google.maps.LatLng(74,-180);
        map_sw =  new google.maps.LatLng(-90,180);
        map_se1 = new google.maps.LatLng(map_sw.lat(),0);
        map_se2 = new google.maps.LatLng(map_sw.lat(),-180);
        map_nw = new google.maps.LatLng(map_ne.lat(),map_sw.lng());
        if(data.bounding_box)data.bounding_box.setMap(null);
        data.bounding_box = new google.maps.Polygon({
        paths: [ne,se,sw,nw,map_ne,map_se2,map_se1,map_sw,map_c,ne],
                  strokeColor: "blue",
                  strokeOpacity: 0.8,
                  strokeWeight: 0,
                  fillColor: "blue",
                  fillOpacity: 0.15,
                  map: data.map
        });
	if (bbox_handler) {
		bbox_handler(response);
	}
    });
}

function addClient(handler){
    //at a a minimum id, lat, lon must be provided
    $.post(host+'geo/set',client,
        function(obj) {
            var response = $.parseJSON(obj);
            client.geonum = obj.geonum;
	    if (handler) {
		handler();
	    }
        }
    );
}

function placeMarker(obj,draggable){
    //make sure we are not leaving duplicate markers
    if(data.arrCurrentMarkers["m"+obj.id]){data.arrCurrentMarkers["m"+obj.id].setMap(null);}
    var el = '<div class="map_info">';
    var userFullName = obj.name;
    if (userFullName.length > 20) {
            userFullName = userFullName.substr(0, 20) + '...';
    }
	el += '<img class="profileimg" src="'+obj.imageURL+'"/>';
	el += '<div><p>'+userFullName+'</p></div>';
	el += '<span class="links">';
	el += '<a target="_blank" href="'+obj.profileURL+'">Profile ';
	if(typeof(obj.facebookid) != "undefined" && obj.facebookid != 0)el += ' <img class="fbimg" src="shout_facebook.png" alt="Facebook">';
	el += '</a>';
	el += '</div>';
	var infowindow = new InfoBox({
		disableAutoPan: false,
		maxWidth: 150,
		zIndex: null,
		boxStyle: {
			opacity: 1,
			width: "280px"
		},
		closeBoxMargin: "12px 4px 2px 2px",
		closeBoxURL: "http://www.google.com/intl/en_us/mapfiles/close.gif",
		infoBoxClearance: new google.maps.Size(1, 1),
		content: el
	});

    if(draggable){
        //Client
        var marker = new google.maps.Marker({
            icon: 'http://maps.google.com/mapfiles/ms/icons/blue-dot.png',
            draggable: true,
            position: new google.maps.LatLng(obj.lat, obj.lon),
            map: data.map
         });
         //show the infowindow with the new data
         data.clientinfo = infowindow;
         data.clientinfo.open(data.map,marker);
         setTimeout(function () { data.clientinfo.close(); }, 2000);
         google.maps.event.addListener(marker, 'dragend', function() {
             if(data.clientinfo)data.clientinfo.close();
              var pos = marker.getPosition();
              client.lat = pos.lat();
              client.lon = pos.lng();
              addClient(joinArea);
              if(data.bounding_box)data.bounding_box.setMap(null);
        });
    }else{
        //Neighbor
       var marker = new google.maps.Marker({
        position: new google.maps.LatLng(obj.lat, obj.lon),
        map: data.map
       });
    }
    google.maps.event.addListener(marker, 'click', function() {
        infowindow.open(data.map,marker);
        setTimeout(function () { infowindow.close(); }, 10000);
    });

    data.arrCurrentMarkers["m"+obj.id] = marker; //Add Marker to array
}

function removeClient(id){
    $.post(host+'geo/delete','id='+id,
        function(response) {
            removeMarker(id);
        }
    );
    if (connection && connection.connected) {
        // ensure signout
        $.ajax({
                type: 'POST',
                url: BOSH_SERVICE,
                async: false,
                cache: false,
                contentType: 'application/xml',
                data: "<body rid='" + connection.rid + "' xmlns='http://jabber.org/protocol/httpbind' sid='" + connection.sid + "' type='terminate'><presence xmlns='jabber:client' type='unavailable'/></body>",
                success: function(data) {
                console.log('signed out');
                console.log(data);
            },
            error: function(XMLHttpRequest, textStatus, errorThrown) {
                console.log('signout error', textStatus + ' (' + errorThrown + ')');
            }
        });
    }
}

function removeMarker (id) {
     if(data.arrCurrentMarkers["m"+id]){
        data.arrCurrentMarkers["m"+id].setMap(null);
        delete 	data.arrCurrentMarkers["m"+id];
	removeNeighbor(id);
     }
}

function clearMap() {
    for(var key in data.arrCurrentMarkers){
            data.arrCurrentMarkers[key].setMap(null);
            delete data.arrCurrentMarkers[key];
    }
    placeMarker(client,true);
}//end clearMap

// XMPP

function connect_xmpp() {
   RTC = setupRTC();
   connection = new Strophe.Connection(BOSH_SERVICE);
    if (RAWLOGGING) {
        connection.rawInput = function(data) { console.log('RECV: ' + data); };
        connection.rawOutput = function(data) { console.log('SEND: ' + data); };
    }
    connection.jingle.PRANSWER = PRANSWER;
    connection.jingle.AUTOACCEPT = AUTOACCEPT;
    connection.jingle.ice_config = ice_config;
    connection.jingle.MULTIPARTY = MULTIPARTY;
    connection.jingle.pc_constraints = RTC.pc_constraints;
    connection.connect(client.id + '@' + DOMAIN, "", onConnect);
}


function onConnect(status) {
    if (status == Strophe.Status.CONNECTING) {
        setStatus('Connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
        setStatus('Connecting failed.');
    } else if (status == Strophe.Status.DISCONNECTING) {
        setStatus('Disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
        setStatus('Disconnected.');
        if (localStream) {
            localStream.stop();
            localStream = null;
        }
    } else if (status == Strophe.Status.CONNECTED) {
        setStatus('Connected.');
	// Get TURN/STUN credentials
	turn_service_connection = new Strophe.Connection(TURN_CREDENTIALS_SERVICE);
    	if (RAWLOGGING) {
        	turn_service_connection.rawInput = function(data) { console.log('TURN RECV: ' + data); };
        	turn_service_connection.rawOutput = function(data) { console.log('TURN SEND: ' + data); };
    	}
	turn_service_connection.connect(TURN_DOMAIN, null, onTurnConnect);
	// Add permanent handler (for 'available' presence)
        connection.addHandler(onNeighborIn, null, 'presence');
	// Join the area
	joinArea();
    }
}

function onTurnConnect(status) {
    if (status == Strophe.Status.CONNECTING) {
        setStatus('TURN/STUN service: Connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
        setStatus('TURN/STUN service: Connecting failed.');
    } else if (status == Strophe.Status.DISCONNECTING) {
        setStatus('TURN/STUN service: Disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
        setStatus('TURN/STUN service: Disconnected.');
    } else if (status == Strophe.Status.CONNECTED) {
        setStatus('TURN/STUN service connected.');
        turn_service_connection.jingle.getStunAndTurnCredentials(onGetStunAndTurn);
	}
}

function onGetStunAndTurn(iceservers) {
        
        connection.jingle.ice_config.iceServers  = iceservers;
        // disco stuff
        if (connection.disco) {
            connection.disco.addIdentity('client', 'web');
            connection.disco.addFeature(Strophe.NS.DISCO_INFO);
        }
	// Don't need TURN service, disconnect
	turn_service_connection.disconnect(); 
}

function setStatus(txt) {
    console.log('status', txt);
}

function joinArea() {
	getBoundingBox(function(response) {
		// Remove old handlers, if any
		removePresenceHandlers();
                // Set up presence listeners for the "home" area
		// Note: the listener for incoming 'available' presences being registered upon connection, as it doesn't depend on the "home room"
		// TODO: consider remove dependency on "home room" and have the listener that ignores noise outside the grid
                var home_room_jid = client.geonum + '@' + CONFERENCEDOMAIN  + '/' + client.id;
                handlers.push(connection.addHandler(onNeighborOut, null, 'presence', 'unavailable', null, home_room_jid, {matchBare: true}));
                handlers.push(connection.addHandler(onPresenceError, null, 'presence', 'error', null, home_room_jid, {matchBare: true}));

		// Figure out what areas to notify
		// We could have moved to the new location, so we need to figure:
		// 1) what areas we've left - need to send 'unavailable' to them;
		// 2) what areas we are joining - need to send 'available';
		// 3) ignore other areas (i.e. the ones that we are still in).
		console.log("The current tiles:" + client.tiles);
		console.log("The new tiles:" + response.tiles);
		
		client.tiles.forEach(function(t) {
                        if (response.tiles.indexOf(t) == -1) { //area to leave
                                var roomjid = t + '@' + CONFERENCEDOMAIN + '/' + client.id;
                                // Leave the room
				console.log("Leaving " + roomjid);
				removeMarkers(t);
                                pres = $pres({to: roomjid, type: 'unavailable' })
                                        .c('x', {xmlns: 'http://jabber.org/protocol/muc'});
                                connection.send(pres);
                        }
                });
		response.tiles.forEach(function(t) {
			if (client.tiles.indexOf(t) == -1) { //new area
				var roomjid = t + '@' + CONFERENCEDOMAIN + '/' + client.id;
		                // Join the room
				console.log("Joining " + roomjid);
                        	pres = $pres({to: roomjid })
                                	.c('x', {xmlns: 'http://jabber.org/protocol/muc'});
                        	connection.send(pres);
			}
		});
	
		// Update the tiles
		client.tiles = response.tiles;
	});
}

function removePresenceHandlers() {
   for (var i = 0; i< handlers.length; i++) {
	if (handlers[i]) {
	   connection.deleteHandler(handlers[i]);
	}	
   }
}

function onNeighborIn(pres) {
   if (!pres) return true;
   var from = pres.getAttribute('from'),
   	type = pres.getAttribute('type');
	console.log("incoming presence from "  + from + ", type =" + type);
    if (Strophe.getDomainFromJid(from) != CONFERENCEDOMAIN) { // only interested in MUC presence
	return true;
    }
    if (type != null) {
        return true;
    }
    if (client.tiles.indexOf(parseInt(Strophe.getNodeFromJid(from))) == -1) { // The presence from outside the grid
	return true;
    }
    var neighborId = Strophe.getResourceFromJid(from);
    if (neighborId && neighborId != client.id) {
        console.log('Neighbor ' + neighborId + ' has come online...');
	getNeighborInfo(neighborId);
    }
    return true;
}

function onNeighborOut(pres) {
   var from = pres.getAttribute('from'),
        type = pres.getAttribute('type');
	console.log("incoming unavailable presence from " + from + ", type =" + type);
    var neighborId = Strophe.getResourceFromJid(from);
    if (neighborId && neighborId != client.id) {
        console.log('Neighbor ' + neighborId + ' has left...');
        removeMarker(neighborId);
    }
    return true;
}

function onPresenceError(pres) {
   console.log("Presence error:" + pres);
   return true;
}

function removeMarkers(geonum) {
   Object.keys(neighbors).forEach(function(k) {if (neighbors[k] == geonum) {removeMarker(k)}});   
}
