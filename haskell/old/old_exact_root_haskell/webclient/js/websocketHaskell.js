function websocketHaskell(){
    const httpPort = location.port;
    const httpHostname = location.hostname;
    const param = location.pathname;
    // var wsUrl = "ws://" + httpHostname + ":" + httpPort + "/" + param;
    var wsUrl = "ws://localhost:9160"
    console.log("Connected to " + wsUrl);
    var wsHaskell = new WebSocket(wsUrl);
    $("#resultHaskell").html("Ergebniss von Haskell: ")
    createWebsocketHaskell(wsHaskell);
    $("#sendenHaskell").on("click", function(e){
      var rad = $("#radicandHaskell").val()
      wsHaskell.send(JSON.stringify(
        {
          jsonrpc: "2.0", 
          method: "exact_root", 
          params: {radikand : parseInt(rad)}, 
          id: 1
        }))
    })
  }
  
  function createWebsocketHaskell(ws) {
    ws.onopen = function() {
      console.log("Websocket on OPEN")
      console.log("ws " + ws.url)
      $('#serverStatusHaskell').text('open');
    }
    // {"result":{"multiplikator":-1,"wurzelwert":5,"radikand":-1},"jsonrpc":"2.0","id":1}
    ws.onmessage = function (evt) {
      console.log("Server sendet : " + evt.data)
      var received_msg = JSON.parse(evt.data)
      console.log("Received Message: " + evt.data)
      var multiplikator = received_msg.result.multiplikator
      var wurzelwert = received_msg.result.wurzelwert
      var radikand = received_msg.result.radikand
      if(multiplikator == -1 && radikand == -1 && wurzelwert > 0){
        // {"result":{"multiplikator":-1,"wurzelwert":-1,"radikand":21},"jsonrpc":"2.0","id":1}
        $("#resultHaskell").html(
          "Ergebnis: " + wurzelwert + "</span>"
        )
      }else if(multiplikator == -1 && wurzelwert == -1 && radikand > 0){
        // {"result":{"multiplikator":-1,"wurzelwert":-1,"radikand":21},"jsonrpc":"2.0","id":1}
        $("#resultHaskell").html(
          "Ergebnis: " + "&radic;<span style=\"text-decoration: overline\">" + 
          radikand + 
          "</span>"
        )
      }else if (multiplikator == -1 && wurzelwert == -1 && radikand == 0){
        $("#resultHaskell").html(
          "Ergebnis: "  + radikand + 
          "</span>"
        )
      }else if (multiplikator == -1 && wurzelwert == -1 && radikand == -1){
        $("#resultHaskell").html(
          "Ergebnis: "  + "unberechenbar" + 
          "</span>"
        )
      }else{
        // {"result":{"multiplikator":5,"wurzelwert":2,"radikand":-1},"jsonrpc":"2.0","id":1}
        // var erg = ""
        // received_msg.result.forEach(element => {
          var erg = "| " + multiplikator + 
          "&times;&radic;<span style=\"text-decoration: overline\">" + 
          wurzelwert + 
          "</span>" + " | "
        // });
        
        $("#resultHaskell").html(
          "Ergebnis: " +  erg
          // multiplikator + 
          // "&times;&radic;<span style=\"text-decoration: overline\">" + 
          // wurzelwert + 
          // "</span>"
        )
      }
    }
  
    ws.onclose = function() {
      $('#serverStatusHaskell').text('closed');
      console.log("Connection is closed...");
    }
}