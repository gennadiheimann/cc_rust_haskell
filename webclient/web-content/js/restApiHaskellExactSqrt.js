function restApiHaskellExactSqrt(){
  $("#resultHaskell").html("Ergebniss von Haskell: ")
  $("#sendenHaskell").on("click", function(e){
    var rad = $("#radicandHaskell").val()
    console.log("Send to Haskell Rest API: " + rad);
      fetch("http://localhost:8082/exactSquareRoot/" + rad)
        .then(response => {
        if (!response.ok) {
            throw new Error('Netzwerk-Antwort war nicht ok');
        }
        return response.json();
        })
        .then(data => {
          console.log("Result from Haskell Rest API: " + JSON.stringify(data));
          var length = data.length;
          if(length == 0){
            $("#resultHaskell").html(
              "Ergebnis: " + "&radic;<span style=\"text-decoration: overline\">" + 
              rad + 
              "</span>"
            );
          } else {
            if(length == 1){
              if (data[0].multiplicator == -1){
                $("#resultHaskell").html(
                  "Ergebnis: " + data[0].squareRoot + 
                  "</span>"
                );
              }else{
                var erg = "| " + data[0].multiplicator + 
                  "&times;&radic;<span style=\"text-decoration: overline\">" + 
                  data[0].squareRoot + 
                  "</span>" + " | ";
                $("#resultHaskell").html(
                  "Ergebnis: " +  erg
                )
              }
            }else{
              var collectRes = "";
              for(const item of data){
                var erg = "| " + item.multiplicator + 
                  "&times;&radic;<span style=\"text-decoration: overline\">" + 
                  item.squareRoot + 
                  "</span>" + " | ";
                  collectRes = collectRes + erg;
              }
              $("#resultHaskell").html(
                  "Ergebnis: " +  collectRes
                )
            }
          }
          })
        .catch(error => {
          document.getElementById('error').textContent = 'Fehler: ' + error;
        });
  })
}