// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { assert } from "../assert/assert.ts";
const DEFAULT_BUFFER_SIZE = 32 * 1024;
/**
 * Copy N size at the most. If read size is lesser than N, then returns nread
 * @param r Reader
 * @param dest Writer
 * @param size Read size
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export async function copyN(r, dest, size) {
  let bytesRead = 0;
  let buf = new Uint8Array(DEFAULT_BUFFER_SIZE);
  while(bytesRead < size){
    if (size - bytesRead < DEFAULT_BUFFER_SIZE) {
      buf = new Uint8Array(size - bytesRead);
    }
    const result = await r.read(buf);
    const nread = result ?? 0;
    bytesRead += nread;
    if (nread > 0) {
      let n = 0;
      while(n < nread){
        n += await dest.write(buf.slice(n, nread));
      }
      assert(n === nread, "could not write");
    }
    if (result === null) {
      break;
    }
  }
  return bytesRead;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL2NvcHlfbi50cyJdLCJzb3VyY2VzQ29udGVudCI6WyIvLyBDb3B5cmlnaHQgMjAxOC0yMDI0IHRoZSBEZW5vIGF1dGhvcnMuIEFsbCByaWdodHMgcmVzZXJ2ZWQuIE1JVCBsaWNlbnNlLlxuLy8gVGhpcyBtb2R1bGUgaXMgYnJvd3NlciBjb21wYXRpYmxlLlxuXG5pbXBvcnQgeyBhc3NlcnQgfSBmcm9tIFwiLi4vYXNzZXJ0L2Fzc2VydC50c1wiO1xuaW1wb3J0IHR5cGUgeyBSZWFkZXIsIFdyaXRlciB9IGZyb20gXCIuL3R5cGVzLnRzXCI7XG5cbmNvbnN0IERFRkFVTFRfQlVGRkVSX1NJWkUgPSAzMiAqIDEwMjQ7XG5cbi8qKlxuICogQ29weSBOIHNpemUgYXQgdGhlIG1vc3QuIElmIHJlYWQgc2l6ZSBpcyBsZXNzZXIgdGhhbiBOLCB0aGVuIHJldHVybnMgbnJlYWRcbiAqIEBwYXJhbSByIFJlYWRlclxuICogQHBhcmFtIGRlc3QgV3JpdGVyXG4gKiBAcGFyYW0gc2l6ZSBSZWFkIHNpemVcbiAqXG4gKiBAZGVwcmVjYXRlZCBUaGlzIHdpbGwgYmUgcmVtb3ZlZCBpbiAxLjAuMC4gVXNlIHRoZSB7QGxpbmsgaHR0cHM6Ly9kZXZlbG9wZXIubW96aWxsYS5vcmcvZW4tVVMvZG9jcy9XZWIvQVBJL1N0cmVhbXNfQVBJIHwgV2ViIFN0cmVhbXMgQVBJfSBpbnN0ZWFkLlxuICovXG5leHBvcnQgYXN5bmMgZnVuY3Rpb24gY29weU4oXG4gIHI6IFJlYWRlcixcbiAgZGVzdDogV3JpdGVyLFxuICBzaXplOiBudW1iZXIsXG4pOiBQcm9taXNlPG51bWJlcj4ge1xuICBsZXQgYnl0ZXNSZWFkID0gMDtcbiAgbGV0IGJ1ZiA9IG5ldyBVaW50OEFycmF5KERFRkFVTFRfQlVGRkVSX1NJWkUpO1xuICB3aGlsZSAoYnl0ZXNSZWFkIDwgc2l6ZSkge1xuICAgIGlmIChzaXplIC0gYnl0ZXNSZWFkIDwgREVGQVVMVF9CVUZGRVJfU0laRSkge1xuICAgICAgYnVmID0gbmV3IFVpbnQ4QXJyYXkoc2l6ZSAtIGJ5dGVzUmVhZCk7XG4gICAgfVxuICAgIGNvbnN0IHJlc3VsdCA9IGF3YWl0IHIucmVhZChidWYpO1xuICAgIGNvbnN0IG5yZWFkID0gcmVzdWx0ID8/IDA7XG4gICAgYnl0ZXNSZWFkICs9IG5yZWFkO1xuICAgIGlmIChucmVhZCA+IDApIHtcbiAgICAgIGxldCBuID0gMDtcbiAgICAgIHdoaWxlIChuIDwgbnJlYWQpIHtcbiAgICAgICAgbiArPSBhd2FpdCBkZXN0LndyaXRlKGJ1Zi5zbGljZShuLCBucmVhZCkpO1xuICAgICAgfVxuICAgICAgYXNzZXJ0KG4gPT09IG5yZWFkLCBcImNvdWxkIG5vdCB3cml0ZVwiKTtcbiAgICB9XG4gICAgaWYgKHJlc3VsdCA9PT0gbnVsbCkge1xuICAgICAgYnJlYWs7XG4gICAgfVxuICB9XG4gIHJldHVybiBieXRlc1JlYWQ7XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQyxTQUFTLE1BQU0sUUFBUSxzQkFBc0I7QUFHN0MsTUFBTSxzQkFBc0IsS0FBSztBQUVqQzs7Ozs7OztDQU9DLEdBQ0QsT0FBTyxlQUFlLE1BQ3BCLENBQVMsRUFDVCxJQUFZLEVBQ1osSUFBWTtFQUVaLElBQUksWUFBWTtFQUNoQixJQUFJLE1BQU0sSUFBSSxXQUFXO0VBQ3pCLE1BQU8sWUFBWSxLQUFNO0lBQ3ZCLElBQUksT0FBTyxZQUFZLHFCQUFxQjtNQUMxQyxNQUFNLElBQUksV0FBVyxPQUFPO0lBQzlCO0lBQ0EsTUFBTSxTQUFTLE1BQU0sRUFBRSxJQUFJLENBQUM7SUFDNUIsTUFBTSxRQUFRLFVBQVU7SUFDeEIsYUFBYTtJQUNiLElBQUksUUFBUSxHQUFHO01BQ2IsSUFBSSxJQUFJO01BQ1IsTUFBTyxJQUFJLE1BQU87UUFDaEIsS0FBSyxNQUFNLEtBQUssS0FBSyxDQUFDLElBQUksS0FBSyxDQUFDLEdBQUc7TUFDckM7TUFDQSxPQUFPLE1BQU0sT0FBTztJQUN0QjtJQUNBLElBQUksV0FBVyxNQUFNO01BQ25CO0lBQ0Y7RUFDRjtFQUNBLE9BQU87QUFDVCJ9
// denoCacheMetadata=12208153533307433456,14237595718869462388