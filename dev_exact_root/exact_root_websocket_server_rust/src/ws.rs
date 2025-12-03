use crate::{Client, Clients};
use futures::{FutureExt, StreamExt};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;
use uuid::Uuid;
use warp::ws::{Message, WebSocket};
use serde::{Serialize, Deserialize};

use crate::exact_root::calc_exact_root::berechne_exacte_wurzel;
use crate::exact_root::calc_exact_root::Res;

pub async fn client_connection(ws: WebSocket, clients: Clients) {
  println!("establishing client connection... {:?}", ws);

  let (client_ws_sender, mut client_ws_rcv) = ws.split();
  let (client_sender, client_rcv) = mpsc::unbounded_channel();

  let client_rcv = UnboundedReceiverStream::new(client_rcv);

  tokio::task::spawn(client_rcv.forward(client_ws_sender).map(|result| {
    if let Err(e) = result {
      println!("error sending websocket msg: {}", e);
    }
  }));

  let uuid = Uuid::new_v4().simple().to_string();

  let new_client = Client {
      client_id: uuid.clone(),
      sender: Some(client_sender),
  };

  clients.lock().await.insert(uuid.clone(), new_client);

  while let Some(result) = client_ws_rcv.next().await {
    let msg = match result {
      Ok(msg) => msg,
      Err(e) => {
        println!("error receiving message for id {}): {}", uuid.clone(), e);
        break;
      }
    };
    client_msg(&uuid, msg, &clients).await;
  }

  clients.lock().await.remove(&uuid);
  println!("{} disconnected", uuid);
}

async fn client_msg(client_id: &str, msg: Message, clients: &Clients) {
  println!("received message from {}: {:?}", client_id, msg);

  let message = match msg.to_str() {
      Ok(v) => v,
      Err(_) => return,
  };

// --> {"jsonrpc": "2.0", "method": "subtract", "params": {"subtrahend": 23, "minuend": 42}, "id": 3}
// <-- {"jsonrpc": "2.0", "result": 19, "id": 3}

// --> {"jsonrpc": "2.0", "method": "subtract", "params": {"minuend": 42, "subtrahend": 23}, "id": 4}
// <-- {"jsonrpc": "2.0", "result": 19, "id": 4}


  #[derive(Serialize, Deserialize, Debug)]
  struct Response {
    jsonrpc: String,
    result: Result,
    id : u32
  }
  #[derive(Serialize, Deserialize, Debug)]
  struct Result {
    multiplikator : u64,
    wurzelwert : u64
  }
  #[derive(Serialize, Deserialize, Debug)]
  struct Request {
    jsonrpc : String,
    method : String,
    params : Params,
    id : u32
  }

  #[derive(Serialize, Deserialize, Debug)]
  struct Params{
    radikand : u64
  }

  let request: Request = serde_json::from_str(&message).unwrap();
  let locked = clients.lock().await;
  match locked.get(client_id) {
    Some(v) => {
      if let Some(sender) = &v.sender {
        match &*request.method {
          "exact_root" => {
            let param = request.params.radikand;
            // let param_number : u64 = match String::from(param).parse(){
            //   Ok(r) => r,
            //   Err(_) => 0
            // };
            let res : Res = berechne_exacte_wurzel(param);
            let response: Response = Response {
              jsonrpc : String::from("2.0"), 
              result : Result{
                multiplikator : res.multiplikator, 
                wurzelwert : res.wurzelwert
              }, 
              id : 1};
            let response_string = serde_json::to_string(&response).unwrap();
            let _ = sender.send(Ok(Message::text(response_string)));
          }
          _ => {
            println!("Error");
            let _ = sender.send(Ok(Message::text("Error")));
          }
        } 
      }
    }
    None => return,
  }
  return;
}