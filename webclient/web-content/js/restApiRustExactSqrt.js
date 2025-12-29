function restApiHaskellExactSquareRoot(){
  $("#resultRust").html("Ergebniss von Rust: ")
  $("#sendenRust").on("click", function(e){
      var rad = $("#radicandRust").val()
      fetch("http://localhost:8083/exactSquareRoot/" + rad)
        .then(response => {
        if (!response.ok) {
            throw new Error('Netzwerk-Antwort war nicht ok');
        }
        return response.json();
        })
        .then(data => {
          console.log("Ergebnis: " + JSON.stringify(data));
          var length = data.length;
          if(length == 0){
            $("#resultRust").html(
              "Ergebnis: " + "&radic;<span style=\"text-decoration: overline\">" + 
              rad + 
              "</span>"
            );
          } else {
            if(length == 1){
              if (data[0].multiplicator == -1){
                $("#resultRust").html(
                  "Ergebnis: " + data[0].sqrt + 
                  "</span>"
                );
              }else{
                var erg = "| " + data[0].multiplicator + 
                  "&times;&radic;<span style=\"text-decoration: overline\">" + 
                  data[0].sqrt + 
                  "</span>" + " | ";
                $("#resultRust").html(
                  "Ergebnis: " +  erg
                )
              }
            }else{
              var collectRes = "";
              for(const item of data){
                var erg = "| " + item.multiplicator + 
                  "&times;&radic;<span style=\"text-decoration: overline\">" + 
                  item.sqrt + 
                  "</span>" + " | ";
                  collectRes = collectRes + erg;
              }
              $("#resultRust").html(
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