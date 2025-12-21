function websocketRust(){
  const httpPort = location.port;
  const httpHostname = location.hostname;
  const param = location.pathname;
  const wsUrl = "ws://localhost:8000/ws"
  //var wsUrl = "ws://" + httpHostname + ":" + httpPort + "/" + param;
  console.log("Connected to " + wsUrl);
  // ws =  new WebSocket("ws://localhost:9160/");
  var wsRust = new WebSocket(wsUrl);
  $("#result").html("Ergebniss: ")
  createWebsocketRust(wsRust);
  $("#sendenRust").on("click", function(e){
    var rad = $("#radicand").val()
    wsRust.send(JSON.stringify(
      {
        jsonrpc: "2.0", 
        method: "exact_root", 
        params: {radikand : parseInt(rad)}, 
        id: 1
      }))
  })
}

function createWebsocketRust(ws) {
  ws.onopen = function() {
    console.log("Websocket on OPEN")
    console.log("ws " + ws.url)
    $('#serverStatusRust').text('open')
  }
  //rust response
  // {"jsonrpc":"2.0","result":{"multiplikator":1,"wurzelwert":5},"id":1}
  ws.onmessage = function (evt) {
    console.log("Server sendet : " + evt.data)
    var received_msg = JSON.parse(evt.data);
    console.log("Received Message: " + evt.data);
    var root = received_msg.result.wurzelwert;
    var multiplier = received_msg.result.multiplikator;
    $("#result").html(
      "Ergebnis: " +  
      multiplier + 
      "&times;&radic;<span style=\"text-decoration: overline\">" + 
      root + 
      "</span>"
    )
  }

  ws.onclose = function() {
    $('#serverStatusRust').text('closed');
    console.log("Connection is closed...");
  }
}