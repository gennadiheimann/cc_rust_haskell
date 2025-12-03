 document.addEventListener('DOMContentLoaded', main, false);
 //haskell 
 // request -> {"jsonrpc":"2.0","method":"exact_root","params":{"radikand":50},"id":1}
 // response -> {"result":{"multiplikator":5,"wurzelwert":2,"radikand":-1},"jsonrpc":"2.0","id":1}
 //rust
 //request -> {"jsonrpc":"2.0","method":"exact_root","params":{"radikand":50},"id":1}
 //response -> {"jsonrpc":"2.0","result":{"multiplikator":5,"wurzelwert":2},"id":1}

function main(){
  websocketRust()
  websocketHaskell()
}