#pragma once

#include <atomic>
#include <cstdint>
#include <functional>
#include <string>
#include <thread>

// Loopback-only UDP listener. Each datagram is passed to the handler on the
// listener thread; a non-empty return value is sent back to the sender.
class UdpServer {
public:
	using Handler = std::function<std::string(const std::string& message)>;

	~UdpServer();

	bool start(uint16_t port, Handler handler);
	void stop();
	bool running() const { return thread_.joinable(); }
	uint16_t port() const { return port_; }

private:
	void loop();

	Handler handler_;
	std::thread thread_;
	std::atomic<bool> quit_{false};
	intptr_t socket_ = -1;
	uint16_t port_ = 0;
};
