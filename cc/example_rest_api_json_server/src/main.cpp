
#include <iostream>
#include <unordered_map>
#include <mutex>

#include "httplib.h"      // Header-only HTTP server
#include <nlohmann/json.hpp>       // nlohmann::json

using json = nlohmann::json;

int main() {
    // In-Memory "Datenbank"
    std::unordered_map<int, json> items;
    int next_id = 1;
    std::mutex mtx;

    // HTTP-Server auf Port 8080
    httplib::Server svr;

    // Health-Check
    svr.Get("/health", [](const httplib::Request& req, httplib::Response& res){
        json body = {
            {"status", "ok"},
            {"timestamp", static_cast<long long>(time(nullptr))}
        };
        res.set_content(body.dump(), "application/json");
    });

    // Liste aller Items
    svr.Get("/items", [&](const httplib::Request& req, httplib::Response& res) {
        std::lock_guard<std::mutex> lock(mtx);
        json arr = json::array();
        for (const auto& [id, obj] : items) {
            json copy = obj;
            copy["id"] = id;
            arr.push_back(copy);
        }
        res.set_content(arr.dump(), "application/json");
    });

    // Einzelnes Item: /items/{id}
    svr.Get(R"(/items/(\d+))", [&](const httplib::Request& req, httplib::Response& res) {
        int id = std::stoi(req.matches[1].str());
        std::lock_guard<std::mutex> lock(mtx);
        if (items.count(id)) {
            json obj = items[id];
            obj["id"] = id;
            res.set_content(obj.dump(), "application/json");
        } else {
            res.status = 404;
            res.set_content(json{{"error", "not found"}}.dump(), "application/json");
        }
    });

    // Item anlegen: POST /items  (JSON: { "name": "...", "price": 12.3 })
    svr.Post("/items", [&](const httplib::Request& req, httplib::Response& res){
        if (req.get_header_value("Content-Type").find("application/json") == std::string::npos) {
            res.status = 400;
            res.set_content(json{{"error", "Content-Type must be application/json"}}.dump(), "application/json");
            return;
        }

        json body;
        try {
            body = json::parse(req.body);
        } catch (...) {
            res.status = 400;
            res.set_content(json{{"error", "invalid JSON"}}.dump(), "application/json");
            return;
        }

        if (!body.contains("name") || !body.contains("price")) {
            res.status = 422;
            res.set_content(json{{"error", "missing fields: name, price"}}.dump(), "application/json");
            return;
        }

        std::lock_guard<std::mutex> lock(mtx);
        int id = next_id++;
        items[id] = json{{"name", body["name"]}, {"price", body["price"]}};

        json created = items[id];
        created["id"] = id;
        res.status = 201;
        res.set_content(created.dump(), "application/json");
    });

    // Einfache CORS-Unterstützung (optional)
    // svr.Options(R"(/(.*))",  {
    //     res.set_header("Access-Control-Allow-Origin", "*");
    //     res.set_header("Access-Control-Allow-Methods", "GET,POST,OPTIONS");
    //     res.set_header("Access-Control-Allow-Headers", "Content-Type");
    // });
    // svr.set_pre_routing_handler( {
    //     res.set_header("Access-Control-Allow-Origin", "*");
    //     return httplib::Server::HandlerResponse::Unhandled;
    // });

    std::cout << "Server startet auf http://localhost:8080\n";
    // Start (blocking)
    svr.listen("0.0.0.0", 8080);
    return 0;
}
