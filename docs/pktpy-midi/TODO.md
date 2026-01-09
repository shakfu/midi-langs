# TODO

## Improve async

- [x] Add pktpy-midi async tests - 20 async tests added (Tests 28-47)
- [ ] Voice naming/listing in `status()`
- [x] `midi.ms(n)` / `midi.yield_ms(n)` helper for cleaner generator syntax
- [ ] Exception handling improvements
- [x] Fix sequential `run()` calls - fixed by adding `uv_async_send` after timer start
