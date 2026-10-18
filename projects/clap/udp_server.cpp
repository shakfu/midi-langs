#include "udp_server.hpp"

#include <vector>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
typedef int socklen_t;
typedef SOCKET SocketT;
const SocketT kInvalidSocket = INVALID_SOCKET;
#define CLOSE_SOCKET closesocket
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int SocketT;
const SocketT kInvalidSocket = -1;
#define CLOSE_SOCKET close
#endif

UdpServer::~UdpServer()
{
	stop();
}

bool UdpServer::start(uint16_t port, Handler handler)
{
	stop();
#ifdef _WIN32
	WSADATA wsa;
	if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0) return false;
#endif
	SocketT fd = socket(AF_INET, SOCK_DGRAM, 0);
	if (fd == kInvalidSocket) {
#ifdef _WIN32
		WSACleanup();
#endif
		return false;
	}

	sockaddr_in addr{};
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK); // never reachable from other machines
	if (bind(fd, (sockaddr*)&addr, sizeof(addr)) != 0) {
		CLOSE_SOCKET(fd);
#ifdef _WIN32
		WSACleanup();
#endif
		return false;
	}

	socket_ = (intptr_t)fd;
	port_ = port;
	handler_ = std::move(handler);
	quit_ = false;
	thread_ = std::thread([this] { loop(); });
	return true;
}

void UdpServer::stop()
{
	if (!thread_.joinable()) return;
	quit_ = true;
	thread_.join();
	CLOSE_SOCKET((SocketT)socket_);
	socket_ = -1;
#ifdef _WIN32
	WSACleanup();
#endif
}

void UdpServer::loop()
{
	const SocketT fd = (SocketT)socket_;
	std::vector<char> buf(65536);
	while (!quit_) {
		// Poll so stop() is noticed within 100 ms; closing a socket does not reliably wake recvfrom.
		fd_set readable;
		FD_ZERO(&readable);
		FD_SET(fd, &readable);
		timeval timeout{0, 100000};
		if (select((int)fd + 1, &readable, nullptr, nullptr, &timeout) <= 0) continue;

		sockaddr_in from{};
		socklen_t fromLen = sizeof(from);
		int n = (int)recvfrom(fd, buf.data(), (int)buf.size(), 0, (sockaddr*)&from, &fromLen);
		if (n <= 0) continue;

		std::string reply = handler_(std::string(buf.data(), n));
		if (!reply.empty()) {
			sendto(fd, reply.data(), (int)reply.size(), 0, (sockaddr*)&from, fromLen);
		}
	}
}
