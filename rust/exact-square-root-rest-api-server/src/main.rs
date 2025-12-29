mod exact_root;

use axum::{
    extract::Path,
    routing::get,
    Json, Router,
};
use serde::Serialize;
use tokio::net::TcpListener;
use tower_http::cors::CorsLayer;
use axum::http::Method;

#[derive(Serialize)]
struct Result {
    multiplicator: i64,
    squareRoot: i64,
}

#[tokio::main]
async fn main() {
    let cors = CorsLayer::new()
          .allow_origin(tower_http::cors::Any)
          .allow_methods([Method::GET])
          .allow_headers(tower_http::cors::Any);
    let app = Router::new()
        .route("/exactSquareRoot/:param", get(exact_square_root).layer(cors));

    let listener = TcpListener::bind("0.0.0.0:8083")
        .await
        .unwrap();

    println!("Server läuft auf http://localhost:8083");

    axum::serve(listener, app).await.unwrap();
}

async fn exact_square_root(Path(param): Path<i64>) -> Json<Vec<Result>> {
  use crate::exact_root::exact_root::get_exact_sqrt;

  let results = get_exact_sqrt(param);
  let mut json_results: Vec<Result> = vec![];
  for r in results {
    json_results.push(Result{multiplicator: r.multiplicator, squareRoot: r.sqrt})
  }
  Json(json_results)
}