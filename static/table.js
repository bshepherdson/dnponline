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
var userVars  = [];

function send(msg) {
    $.ajax({ type: 'POST', url: "/say", data: { message: msg }, success: function(o) {
        if(o.status == "private") {
            display("*** " + o.message);
        }
    } });
}


function checkIn () {
    $.ajax({ dataType: "json", url: "/check", data: { }, cache: false,
    success: function(data,textStatus,xml) {
        if(data.type == "chat") {
            display(data.sender + ": " + data.content);
        } else if(data.type == "board") {
            updateMap(data.tokens);
        } else if(data.type == "whisper") {
            display("Whisper from " + data.sender + ": " + data.content);
        } else if(data.type == "vars") {
            updateVars(data.vars);
        }
    }, 
    error: function() {
        waitBeforeCheckIn = true;
    }, 
    complete: function() {
        needsToCheckIn = true;
    }});
}

function display (msg) {
    var ta = $("#chattextarea");
    ta.html(ta.html() + msg + "<br/>\n");
    //console.log(ta.scrollTop + "," + ta.height + "," + ta.scrollHeight);
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

function updateVars(newVars) {
    var varsHtml = "<table class='vars'><tr class='vars'><td class='vars'>Nick</td><td class='vars'>Variable</td><td class='vars'>Value</td></tr>";

    for(i in newVars){
        var v = newVars[i];
        varsHtml += "<tr class='vars'><td class='vars'>" + v.nick + "</td><td class='vars'>" + v.name + "</td><td class='vars'>" + v.value + "</td></tr>";
        $("#varstable").html(varsHtml);
    }

    userVars = newVars;
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


