// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
const decoder = new TextDecoder();
/**
 * Writer utility for buffering string chunks.
 *
 * @example
 * ```ts
 * import {
 *   copyN,
 *   StringReader,
 *   StringWriter,
 * } from "https://deno.land/std@$STD_VERSION/io/mod.ts";
 * import { copy } from "https://deno.land/std@$STD_VERSION/io/copy.ts";
 *
 * const w = new StringWriter("base");
 * const r = new StringReader("0123456789");
 * await copyN(r, w, 4); // copy 4 bytes
 *
 * // Number of bytes read
 * console.log(w.toString()); //base0123
 *
 * await copy(r, w); // copy all
 * console.log(w.toString()); // base0123456789
 * ```
 *
 * **Output:**
 *
 * ```text
 * base0123
 * base0123456789
 * ```
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export class StringWriter {
  base;
  #chunks;
  #byteLength;
  #cache;
  constructor(base = ""){
    this.base = base;
    this.#chunks = [];
    this.#byteLength = 0;
    const c = new TextEncoder().encode(base);
    this.#chunks.push(c);
    this.#byteLength += c.byteLength;
  }
  write(p) {
    return Promise.resolve(this.writeSync(p));
  }
  writeSync(p) {
    this.#chunks.push(new Uint8Array(p));
    this.#byteLength += p.byteLength;
    this.#cache = undefined;
    return p.byteLength;
  }
  toString() {
    if (this.#cache) {
      return this.#cache;
    }
    const buf = new Uint8Array(this.#byteLength);
    let offs = 0;
    for (const chunk of this.#chunks){
      buf.set(chunk, offs);
      offs += chunk.byteLength;
    }
    this.#cache = decoder.decode(buf);
    return this.#cache;
  }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3N0cmluZ193cml0ZXIudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cbi8vIFRoaXMgbW9kdWxlIGlzIGJyb3dzZXIgY29tcGF0aWJsZS5cblxuaW1wb3J0IHR5cGUgeyBXcml0ZXIsIFdyaXRlclN5bmMgfSBmcm9tIFwiLi90eXBlcy50c1wiO1xuXG5jb25zdCBkZWNvZGVyID0gbmV3IFRleHREZWNvZGVyKCk7XG5cbi8qKlxuICogV3JpdGVyIHV0aWxpdHkgZm9yIGJ1ZmZlcmluZyBzdHJpbmcgY2h1bmtzLlxuICpcbiAqIEBleGFtcGxlXG4gKiBgYGB0c1xuICogaW1wb3J0IHtcbiAqICAgY29weU4sXG4gKiAgIFN0cmluZ1JlYWRlcixcbiAqICAgU3RyaW5nV3JpdGVyLFxuICogfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby9tb2QudHNcIjtcbiAqIGltcG9ydCB7IGNvcHkgfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby9jb3B5LnRzXCI7XG4gKlxuICogY29uc3QgdyA9IG5ldyBTdHJpbmdXcml0ZXIoXCJiYXNlXCIpO1xuICogY29uc3QgciA9IG5ldyBTdHJpbmdSZWFkZXIoXCIwMTIzNDU2Nzg5XCIpO1xuICogYXdhaXQgY29weU4ociwgdywgNCk7IC8vIGNvcHkgNCBieXRlc1xuICpcbiAqIC8vIE51bWJlciBvZiBieXRlcyByZWFkXG4gKiBjb25zb2xlLmxvZyh3LnRvU3RyaW5nKCkpOyAvL2Jhc2UwMTIzXG4gKlxuICogYXdhaXQgY29weShyLCB3KTsgLy8gY29weSBhbGxcbiAqIGNvbnNvbGUubG9nKHcudG9TdHJpbmcoKSk7IC8vIGJhc2UwMTIzNDU2Nzg5XG4gKiBgYGBcbiAqXG4gKiAqKk91dHB1dDoqKlxuICpcbiAqIGBgYHRleHRcbiAqIGJhc2UwMTIzXG4gKiBiYXNlMDEyMzQ1Njc4OVxuICogYGBgXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGNsYXNzIFN0cmluZ1dyaXRlciBpbXBsZW1lbnRzIFdyaXRlciwgV3JpdGVyU3luYyB7XG4gICNjaHVua3M6IFVpbnQ4QXJyYXlbXSA9IFtdO1xuICAjYnl0ZUxlbmd0aCA9IDA7XG4gICNjYWNoZTogc3RyaW5nIHwgdW5kZWZpbmVkO1xuXG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgYmFzZTogc3RyaW5nID0gXCJcIikge1xuICAgIGNvbnN0IGMgPSBuZXcgVGV4dEVuY29kZXIoKS5lbmNvZGUoYmFzZSk7XG4gICAgdGhpcy4jY2h1bmtzLnB1c2goYyk7XG4gICAgdGhpcy4jYnl0ZUxlbmd0aCArPSBjLmJ5dGVMZW5ndGg7XG4gIH1cblxuICB3cml0ZShwOiBVaW50OEFycmF5KTogUHJvbWlzZTxudW1iZXI+IHtcbiAgICByZXR1cm4gUHJvbWlzZS5yZXNvbHZlKHRoaXMud3JpdGVTeW5jKHApKTtcbiAgfVxuXG4gIHdyaXRlU3luYyhwOiBVaW50OEFycmF5KTogbnVtYmVyIHtcbiAgICB0aGlzLiNjaHVua3MucHVzaChuZXcgVWludDhBcnJheShwKSk7XG4gICAgdGhpcy4jYnl0ZUxlbmd0aCArPSBwLmJ5dGVMZW5ndGg7XG4gICAgdGhpcy4jY2FjaGUgPSB1bmRlZmluZWQ7XG4gICAgcmV0dXJuIHAuYnl0ZUxlbmd0aDtcbiAgfVxuXG4gIHRvU3RyaW5nKCk6IHN0cmluZyB7XG4gICAgaWYgKHRoaXMuI2NhY2hlKSB7XG4gICAgICByZXR1cm4gdGhpcy4jY2FjaGU7XG4gICAgfVxuICAgIGNvbnN0IGJ1ZiA9IG5ldyBVaW50OEFycmF5KHRoaXMuI2J5dGVMZW5ndGgpO1xuICAgIGxldCBvZmZzID0gMDtcbiAgICBmb3IgKGNvbnN0IGNodW5rIG9mIHRoaXMuI2NodW5rcykge1xuICAgICAgYnVmLnNldChjaHVuaywgb2Zmcyk7XG4gICAgICBvZmZzICs9IGNodW5rLmJ5dGVMZW5ndGg7XG4gICAgfVxuICAgIHRoaXMuI2NhY2hlID0gZGVjb2Rlci5kZWNvZGUoYnVmKTtcbiAgICByZXR1cm4gdGhpcy4jY2FjaGU7XG4gIH1cbn1cbiJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSwwRUFBMEU7QUFDMUUscUNBQXFDO0FBSXJDLE1BQU0sVUFBVSxJQUFJO0FBRXBCOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0NBK0JDLEdBQ0QsT0FBTyxNQUFNOztFQUNYLENBQUEsTUFBTyxDQUFvQjtFQUMzQixDQUFBLFVBQVcsQ0FBSztFQUNoQixDQUFBLEtBQU0sQ0FBcUI7RUFFM0IsWUFBWSxBQUFRLE9BQWUsRUFBRSxDQUFFO1NBQW5CLE9BQUE7U0FKcEIsQ0FBQSxNQUFPLEdBQWlCLEVBQUU7U0FDMUIsQ0FBQSxVQUFXLEdBQUc7SUFJWixNQUFNLElBQUksSUFBSSxjQUFjLE1BQU0sQ0FBQztJQUNuQyxJQUFJLENBQUMsQ0FBQSxNQUFPLENBQUMsSUFBSSxDQUFDO0lBQ2xCLElBQUksQ0FBQyxDQUFBLFVBQVcsSUFBSSxFQUFFLFVBQVU7RUFDbEM7RUFFQSxNQUFNLENBQWEsRUFBbUI7SUFDcEMsT0FBTyxRQUFRLE9BQU8sQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDO0VBQ3hDO0VBRUEsVUFBVSxDQUFhLEVBQVU7SUFDL0IsSUFBSSxDQUFDLENBQUEsTUFBTyxDQUFDLElBQUksQ0FBQyxJQUFJLFdBQVc7SUFDakMsSUFBSSxDQUFDLENBQUEsVUFBVyxJQUFJLEVBQUUsVUFBVTtJQUNoQyxJQUFJLENBQUMsQ0FBQSxLQUFNLEdBQUc7SUFDZCxPQUFPLEVBQUUsVUFBVTtFQUNyQjtFQUVBLFdBQW1CO0lBQ2pCLElBQUksSUFBSSxDQUFDLENBQUEsS0FBTSxFQUFFO01BQ2YsT0FBTyxJQUFJLENBQUMsQ0FBQSxLQUFNO0lBQ3BCO0lBQ0EsTUFBTSxNQUFNLElBQUksV0FBVyxJQUFJLENBQUMsQ0FBQSxVQUFXO0lBQzNDLElBQUksT0FBTztJQUNYLEtBQUssTUFBTSxTQUFTLElBQUksQ0FBQyxDQUFBLE1BQU8sQ0FBRTtNQUNoQyxJQUFJLEdBQUcsQ0FBQyxPQUFPO01BQ2YsUUFBUSxNQUFNLFVBQVU7SUFDMUI7SUFDQSxJQUFJLENBQUMsQ0FBQSxLQUFNLEdBQUcsUUFBUSxNQUFNLENBQUM7SUFDN0IsT0FBTyxJQUFJLENBQUMsQ0FBQSxLQUFNO0VBQ3BCO0FBQ0YifQ==
// denoCacheMetadata=12257344761892989232,16510258969224124955