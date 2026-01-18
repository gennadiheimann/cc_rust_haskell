#include <iostream>
#include <unordered_map>
#include <mutex>

#include "httplib.h"      // Header-only HTTP server
#include <nlohmann/json.hpp>       // nlohmann::json
#include "exact_square_root.h"

using json = nlohmann::json;

int main() {
    std::mutex mtx;

    httplib::Server svr;

    svr.Get(R"(/exactSquareRoot/(\d+))", [&](const httplib::Request& req, httplib::Response& res) {
        // res.set_header("Access-Control-Allow-Origin", "*");
        int radicand = std::stoi(req.matches[1].str());
        std::lock_guard<std::mutex> lock(mtx);
        exact_square_root::ExactSquareRoot esr{radicand};
        std::vector<exact_square_root::Res> results = esr.GetExactSqrt();
        json jresults = json::array();
        for(const auto& result : results){
          json jresult{
            {"multiplicator", result.multiplicator},
            {"squareRoot", result.square_root}
          };
          jresults.push_back(jresult);
        }
        res.set_content(jresults.dump(), "application/json");
    });

    svr.Options(R"(/(.*))", [&](const httplib::Request& req, httplib::Response& res) {
        res.set_header("Access-Control-Allow-Origin", "*");
        res.set_header("Access-Control-Allow-Methods", "GET,POST,OPTIONS");
        res.set_header("Access-Control-Allow-Headers", "Content-Type");
    });

    svr.set_pre_routing_handler([](const httplib::Request& req, httplib::Response& res){
        res.set_header("Access-Control-Allow-Origin", "*");
        return httplib::Server::HandlerResponse::Unhandled;
    });

    std::cout << "Server startet auf http://localhost:8081\n";
    svr.listen("0.0.0.0", 8081);
    return 0;
}
