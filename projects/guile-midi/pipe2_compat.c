/* pipe2 for static libguile on macOS before 27.
 *
 * Homebrew's libguile-3.0.a calls pipe2 but leaves out gnulib's replacement, which its
 * dylib carries. The macOS 27 SDK exports pipe2 from libSystem, so the link succeeds, and
 * dyld then aborts on macOS 26 and earlier. Defining it here binds libguile's calls to this
 * copy. Like gnulib's fallback, it is not atomic: another thread can fork between pipe and
 * FD_CLOEXEC. */

#include <AvailabilityMacros.h>

#if MAC_OS_X_VERSION_MIN_REQUIRED < 270000

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

int pipe2(int fds[2], int flags)
{
	if (flags & ~(O_CLOEXEC | O_NONBLOCK)) {
		errno = EINVAL;
		return -1;
	}
	int tmp[2];
	if (pipe(tmp) != 0) return -1;
	for (int i = 0; i < 2; i++) {
		if (((flags & O_CLOEXEC) && fcntl(tmp[i], F_SETFD, FD_CLOEXEC) != 0)
		    || ((flags & O_NONBLOCK) && fcntl(tmp[i], F_SETFL, fcntl(tmp[i], F_GETFL) | O_NONBLOCK) != 0)) {
			int saved = errno;
			close(tmp[0]);
			close(tmp[1]);
			errno = saved;
			return -1;
		}
	}
	fds[0] = tmp[0];
	fds[1] = tmp[1];
	return 0;
}

#endif
