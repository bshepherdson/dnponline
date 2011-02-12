$(document).ready(function () {
    $("#chatform").submit(function (e) {
        e.preventDefault();
        send($("#chatmessage").attr("value"));

        // shuffle the new command into the history
        historyIndex = -1;
        var oldTemp = $("#chatmessage").attr("value");
        var newTemp;
        for(var i = 0; i < chatHistory.length && i < 10; i++) {
            newTemp = chatHistory[i];
            chatHistory[i] = oldTemp;
            oldTemp = newTemp;
        }
        chatHistory[i] = oldTemp;

        // and clear the field
        $("#chatmessage").attr("value", "");
    });

    $("#chatform").bind("ajaxComplete", function(e) {
        if(needsToCheckIn) {
            needsToCheckIn = false;
            if(waitBeforeCheckIn) {
                waitBeforeCheckIn = false;
                var junk = setTimeout(function() { checkIn(); }, 5000); // wait 5 seconds
            } else {
                checkIn();
            }
        }
    });

    chatMessage = $("#chatmessage");
    chatMessage.keydown(keyHandler);

    // attach the click() event to all the td cells
    $("td.grid").click(clickHandler);

    $("#d4button").click(rollDice(4));
    $("#d6button").click(rollDice(6));
    $("#d8button").click(rollDice(8));
    $("#d10button").click(rollDice(10));
    $("#d12button").click(rollDice(12));
    $("#d20button").click(rollDice(20));
    $("#d100button").click(rollDice('%'));

    $("#chatscrolllock").click(scrollLock);

    checkIn();
});

var needsToCheckIn = false;
var waitBeforeCheckIn = false;

var mapTokens = [];

function send(msg) {
    $.ajax({ type: 'POST', url: "/say", data: { message: msg }, success: function(o) {
        if(o.status == "private") {
            display("<span class=\"private\">*** " + o.message + "</span>");
        }
    } });
}


function checkIn () {
    $.ajax({ dataType: "json", url: "/check", data: { }, cache: false,
    success: function(data,textStatus,xml) {
        if(data.type == "chat") {
            var color = getColor(data.sender);
            display("<span style=\"color: " + color + "\">"+data.sender + ":</span> " + data.content);
        } else if(data.type == "board") {
            updateMap(data.tokens);
        } else if(data.type == "whisper") {
            var color = getColor(data.sender);
            display("<span class=\"whisper\">Whisper from <span style=\"color: " + color + "\">" + data.sender + ":</span> " + data.content + "</span>");
        } else if(data.type == "vars") {
            updateVars(data.vars);
        } else if(data.type == "junk") {
            // do nothing
        } else if(data.type == "colors") {
            userColors = {};
            for(var i = 0; i < data.colors.length; i++) {
                userColors[data.colors[i][0]] = data.colors[i][1];
            }
        }
    }, 
    error: function() {
        waitBeforeCheckIn = true;
    }, 
    complete: function() {
        needsToCheckIn = true;
    }});
}

var userColors = {};
function getColor (sender) {
    if(userColors && userColors[sender]){
        return userColors[sender];
    } else {
        return "#cccccc";
    }
}


function display (msg) {
    var ta = $("#chattextarea");
    ta.html(ta.html() + msg + "<br/>\n");
    if(!scrollLocked) {
        ta.scrollTop(100000000);
    } else {
        ta.addClass('borderhighlight');
        ta.removeClass('bordernormal');
    }
}


function updateMap(newTokens) {
    // first hack: just remove them all and replace them with the new ones
    $("td.grid").html("");

    for (i in newTokens) {
        var t = newTokens[i];
        var squareId = "sq_" + t.x + "x" + t.y;
        var square = $("#"+squareId);
        square.html("<img class=\"grid\" src=\"/static/images/" + t.image + "\" />");
    }

    mapTokens = newTokens;
}



var vartables = {};

Vartable = function(nick, vars) {
    this.nick = nick;
    this.vars = vars;
    this.visible = false;
}


function updateVars(newVars) {
    for(var i = 0; i < newVars.length; i++) {
        if(vartables[newVars[i].nick]) {
            vartables[newVars[i].nick].vars = newVars[i].vars;
        } else {
            vartables[newVars[i].nick] = new Vartable(newVars[i].nick, newVars[i].vars);
        }
    }

    var nicks = Object.keys(vartables);
    console.log(nicks);
    console.log(vartables);
    if(nicks) {
        nicks.sort();

        var varsHtml = "";
        for(var i = 0; i < nicks.length; i++) {
            var v = vartables[nicks[i]];
            varsHtml += "<div class='vars'>";
            varsHtml += "<h4 class='vars'><a href=\"javascript:toggleVarsTable('"+ v.nick +"')\">" + v.nick + "</a></h4>";
            varsHtml += "<div class=\"varstable";
            varsHtml += v.visible ? "" : " hidden";
            varsHtml += "\" id=\"" + v.nick + "table\">";
            varsHtml += "<table class=\"vars\">";
            for(var j = 0; j < v.vars.length; j++) {
                varsHtml += "<tr class=\"vars\">";
                varsHtml += "<td class=\"vars\">" + v.vars[j][0] + "</td>";
                varsHtml += "<td class=\"vars\">" + v.vars[j][1] + "</td>";
                varsHtml += "</tr>";
            }
            varsHtml += "</table></div></div>";
        }

        $("#varstable").html(varsHtml);
    }
}


function toggleVarsTable(nick) {
    if(!vartables[nick]) return;

    if(vartables[nick].visible){
        $("#"+nick+"table").addClass("hidden");
        vartables[nick].visible = false;
    } else {
        $("#"+nick+"table").removeClass("hidden");
        vartables[nick].visible = true;
    }
}

function collapseAllVars() {
    expandCollapseAllVars(false, function(v){ v.addClass('hidden'); });
}

function expandAllVars() {
    expandCollapseAllVars(true, function(v){ v.removeClass('hidden'); });
}

function expandCollapseAllVars(visible, f) {
    nicks = Object.keys(vartables);
    if(nicks) {
        for(var i = 0; i < nicks.length; i++) {
            nicks[i].visible = visible;
        }
    }

    f($("div.varstable"));
}



var chatHistory = [];
var historyIndex = -1;
var currentCommand = null;
var chatMessage;

function keyHandler(e) {
    if(e.keyCode == 38) { // up
        e.preventDefault();

        if(historyIndex < chatHistory.length-1) {
            if(historyIndex < 0) {
                currentCommand = chatMessage.attr("value"); // store the current command
            }
            historyIndex++;
            chatMessage.attr("value", chatHistory[historyIndex]);
            chatMessage[0].setSelectionRange(10000,10000); // put cursor at the right edge
        }
    } else if(e.keyCode == 40) { //down
        e.preventDefault();

        var value;
        if(historyIndex == 0) { 
            historyIndex = -1;
            value = currentCommand;
        } else if(historyIndex > 0) {
            historyIndex--;
            value = chatHistory[historyIndex];
        }
        chatMessage.attr("value", value);
        chatMessage[0].setSelectionRange(10000,10000); // put cursor at the right edge.
    }
}

var selected = null;

function clickHandler(e) {
    var id = e.currentTarget.id;
    var x = id.slice(id.indexOf("_")+1, id.indexOf("x"));
    var y = id.slice(id.indexOf("x")+1);

    if(selected){
        if(selected.x == x && selected.y == y){
            // de-select and de-highlight
            selected = null;
            $("td.gridhighlight").removeClass("gridhighlight");
        } else {
            // move the selected element to the given location with a /place command
            var cmd = "/place " + x + " " + y + " " + selected.image + " " + selected.name;
            $("td.gridhighlight").removeClass("gridhighlight");
            selected = null;
            send(cmd);
        }
    } else {
        // check if there's an image under the spot where we clicked
        for(i in mapTokens) {
            if(mapTokens[i].x == x && mapTokens[i].y == y) {
                selected = mapTokens[i];
            }
        }

        if(selected){
            $("#sq_" + x + "x" + y).addClass("gridhighlight");
        }

    }

}


var gridVisible = false;

function toggleGridVisibility() {
    if(gridVisible) {
        gridVisible = false;
        $("#gridpane").addClass("hidden");
        $("#gridhideshow").html("Show grid");
    } else {
        gridVisible = true;
        $("#gridpane").removeClass("hidden");
        $("#gridhideshow").html("Hide grid");
    }
}



function rollDice(n) {
    return function (e) {
        send("/d"+n);
    };
}


var scrollLocked = false;

function scrollLock() {
    if(scrollLocked) {
        scrollLocked = false;
        var ta = $("#chattextarea");
        ta.scrollTop(100000000);
        ta.addClass('bordernormal');
        ta.removeClass('borderhighlight');
        $("#chatscrolllock").attr('src', "/static/images/blue.png");
    } else {
        scrollLocked = true;
        $("#chatscrolllock").attr('src', "/static/images/red.png");
    }
}


