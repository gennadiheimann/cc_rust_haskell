function restApiHaskellExactSquareRoot(){
    $("#resultHaskell").html("Ergebniss von Haskell: ")
    $("#sendenHaskell").on("click", function(e){
        var rad = $("#radicandHaskell").val()
        // var xhr = new XMLHttpRequest();
        // xhr.open("GET", "http://localhost:8082/exactSquareRoot/" + rad);
        // xhr.onreadystatechange = function() {
        // if (xhr.readyState === 4) {
        //     if (xhr.status === 200) {
        //     try {
        //         var data = JSON.parse(xhr.responseText);
        //         // Ergebnis anzeigen (z.B. in einem HTML-Element)
        //         // document.getElementById('result').textContent = JSON.stringify(data);
        //         console.log("Ergebnis: " + JSON.stringify(data));
        //     } catch (e) {
        //         document.getElementById('result').textContent = 'Fehler: ' + e;
        //     }
        //     } else {
        //     document.getElementById('result').textContent = 'Fehler: ' + xhr.statusText;
        //     }
        // }
        // };
        // xhr.send();
        fetch("http://localhost:8082/exactSquareRoot/" + rad)
            .then(response => {
            if (!response.ok) {
                throw new Error('Netzwerk-Antwort war nicht ok');
            }
            return response.json();
            })
            .then(data => {
            // Ergebnis anzeigen (z.B. in einem HTML-Element)
            // document.getElementById('result').textContent = JSON.stringify(data);
            console.log("Ergebnis: " + JSON.stringify(data));
            })
            .catch(error => {
            document.getElementById('result').textContent = 'Fehler: ' + error;
            });
    })
  
}