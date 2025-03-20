// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
import { copy as copyBytes } from "../bytes/copy.ts";
import { assert } from "../assert/assert.ts";
const DEFAULT_BUFFER_SIZE = 32 * 1024;
/**
 * Read a range of bytes from a file or other resource that is readable and
 * seekable.  The range start and end are inclusive of the bytes within that
 * range.
 *
 * ```ts
 * import { assertEquals } from "https://deno.land/std@$STD_VERSION/assert/assert_equals.ts";
 * import { readRange } from "https://deno.land/std@$STD_VERSION/io/read_range.ts";
 *
 * // Read the first 10 bytes of a file
 * const file = await Deno.open("example.txt", { read: true });
 * const bytes = await readRange(file, { start: 0, end: 9 });
 * assertEquals(bytes.length, 10);
 * ```
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export async function readRange(r, range) {
  // byte ranges are inclusive, so we have to add one to the end
  let length = range.end - range.start + 1;
  assert(length > 0, "Invalid byte range was passed.");
  await r.seek(range.start, Deno.SeekMode.Start);
  const result = new Uint8Array(length);
  let off = 0;
  while(length){
    const p = new Uint8Array(Math.min(length, DEFAULT_BUFFER_SIZE));
    const nread = await r.read(p);
    assert(nread !== null, "Unexpected EOF reach while reading a range.");
    assert(nread > 0, "Unexpected read of 0 bytes while reading a range.");
    copyBytes(p, result, off);
    off += nread;
    length -= nread;
    assert(length >= 0, "Unexpected length remaining after reading range.");
  }
  return result;
}
/**
 * Read a range of bytes synchronously from a file or other resource that is
 * readable and seekable.  The range start and end are inclusive of the bytes
 * within that range.
 *
 * ```ts
 * import { assertEquals } from "https://deno.land/std@$STD_VERSION/assert/assert_equals.ts";
 * import { readRangeSync } from "https://deno.land/std@$STD_VERSION/io/read_range.ts";
 *
 * // Read the first 10 bytes of a file
 * const file = Deno.openSync("example.txt", { read: true });
 * const bytes = readRangeSync(file, { start: 0, end: 9 });
 * assertEquals(bytes.length, 10);
 * ```
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export function readRangeSync(r, range) {
  // byte ranges are inclusive, so we have to add one to the end
  let length = range.end - range.start + 1;
  assert(length > 0, "Invalid byte range was passed.");
  r.seekSync(range.start, Deno.SeekMode.Start);
  const result = new Uint8Array(length);
  let off = 0;
  while(length){
    const p = new Uint8Array(Math.min(length, DEFAULT_BUFFER_SIZE));
    const nread = r.readSync(p);
    assert(nread !== null, "Unexpected EOF reach while reading a range.");
    assert(nread > 0, "Unexpected read of 0 bytes while reading a range.");
    copyBytes(p, result, off);
    off += nread;
    length -= nread;
    assert(length >= 0, "Unexpected length remaining after reading range.");
  }
  return result;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3JlYWRfcmFuZ2UudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cblxuaW1wb3J0IHsgY29weSBhcyBjb3B5Qnl0ZXMgfSBmcm9tIFwiLi4vYnl0ZXMvY29weS50c1wiO1xuaW1wb3J0IHsgYXNzZXJ0IH0gZnJvbSBcIi4uL2Fzc2VydC9hc3NlcnQudHNcIjtcbmltcG9ydCB0eXBlIHsgUmVhZGVyLCBSZWFkZXJTeW5jIH0gZnJvbSBcIi4vdHlwZXMudHNcIjtcblxuY29uc3QgREVGQVVMVF9CVUZGRVJfU0laRSA9IDMyICogMTAyNDtcblxuLyoqXG4gKiBAZGVwcmVjYXRlZCBUaGlzIHdpbGwgYmUgcmVtb3ZlZCBpbiAxLjAuMC4gVXNlIHRoZSB7QGxpbmsgaHR0cHM6Ly9kZXZlbG9wZXIubW96aWxsYS5vcmcvZW4tVVMvZG9jcy9XZWIvQVBJL1N0cmVhbXNfQVBJIHwgV2ViIFN0cmVhbXMgQVBJfSBpbnN0ZWFkLlxuICovXG5leHBvcnQgaW50ZXJmYWNlIEJ5dGVSYW5nZSB7XG4gIC8qKiBUaGUgMCBiYXNlZCBpbmRleCBvZiB0aGUgc3RhcnQgYnl0ZSBmb3IgYSByYW5nZS4gKi9cbiAgc3RhcnQ6IG51bWJlcjtcblxuICAvKiogVGhlIDAgYmFzZWQgaW5kZXggb2YgdGhlIGVuZCBieXRlIGZvciBhIHJhbmdlLCB3aGljaCBpcyBpbmNsdXNpdmUuICovXG4gIGVuZDogbnVtYmVyO1xufVxuXG4vKipcbiAqIFJlYWQgYSByYW5nZSBvZiBieXRlcyBmcm9tIGEgZmlsZSBvciBvdGhlciByZXNvdXJjZSB0aGF0IGlzIHJlYWRhYmxlIGFuZFxuICogc2Vla2FibGUuICBUaGUgcmFuZ2Ugc3RhcnQgYW5kIGVuZCBhcmUgaW5jbHVzaXZlIG9mIHRoZSBieXRlcyB3aXRoaW4gdGhhdFxuICogcmFuZ2UuXG4gKlxuICogYGBgdHNcbiAqIGltcG9ydCB7IGFzc2VydEVxdWFscyB9IGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL2Fzc2VydC9hc3NlcnRfZXF1YWxzLnRzXCI7XG4gKiBpbXBvcnQgeyByZWFkUmFuZ2UgfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby9yZWFkX3JhbmdlLnRzXCI7XG4gKlxuICogLy8gUmVhZCB0aGUgZmlyc3QgMTAgYnl0ZXMgb2YgYSBmaWxlXG4gKiBjb25zdCBmaWxlID0gYXdhaXQgRGVuby5vcGVuKFwiZXhhbXBsZS50eHRcIiwgeyByZWFkOiB0cnVlIH0pO1xuICogY29uc3QgYnl0ZXMgPSBhd2FpdCByZWFkUmFuZ2UoZmlsZSwgeyBzdGFydDogMCwgZW5kOiA5IH0pO1xuICogYXNzZXJ0RXF1YWxzKGJ5dGVzLmxlbmd0aCwgMTApO1xuICogYGBgXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGFzeW5jIGZ1bmN0aW9uIHJlYWRSYW5nZShcbiAgcjogUmVhZGVyICYgRGVuby5TZWVrZXIsXG4gIHJhbmdlOiBCeXRlUmFuZ2UsXG4pOiBQcm9taXNlPFVpbnQ4QXJyYXk+IHtcbiAgLy8gYnl0ZSByYW5nZXMgYXJlIGluY2x1c2l2ZSwgc28gd2UgaGF2ZSB0byBhZGQgb25lIHRvIHRoZSBlbmRcbiAgbGV0IGxlbmd0aCA9IHJhbmdlLmVuZCAtIHJhbmdlLnN0YXJ0ICsgMTtcbiAgYXNzZXJ0KGxlbmd0aCA+IDAsIFwiSW52YWxpZCBieXRlIHJhbmdlIHdhcyBwYXNzZWQuXCIpO1xuICBhd2FpdCByLnNlZWsocmFuZ2Uuc3RhcnQsIERlbm8uU2Vla01vZGUuU3RhcnQpO1xuICBjb25zdCByZXN1bHQgPSBuZXcgVWludDhBcnJheShsZW5ndGgpO1xuICBsZXQgb2ZmID0gMDtcbiAgd2hpbGUgKGxlbmd0aCkge1xuICAgIGNvbnN0IHAgPSBuZXcgVWludDhBcnJheShNYXRoLm1pbihsZW5ndGgsIERFRkFVTFRfQlVGRkVSX1NJWkUpKTtcbiAgICBjb25zdCBucmVhZCA9IGF3YWl0IHIucmVhZChwKTtcbiAgICBhc3NlcnQobnJlYWQgIT09IG51bGwsIFwiVW5leHBlY3RlZCBFT0YgcmVhY2ggd2hpbGUgcmVhZGluZyBhIHJhbmdlLlwiKTtcbiAgICBhc3NlcnQobnJlYWQgPiAwLCBcIlVuZXhwZWN0ZWQgcmVhZCBvZiAwIGJ5dGVzIHdoaWxlIHJlYWRpbmcgYSByYW5nZS5cIik7XG4gICAgY29weUJ5dGVzKHAsIHJlc3VsdCwgb2ZmKTtcbiAgICBvZmYgKz0gbnJlYWQ7XG4gICAgbGVuZ3RoIC09IG5yZWFkO1xuICAgIGFzc2VydChsZW5ndGggPj0gMCwgXCJVbmV4cGVjdGVkIGxlbmd0aCByZW1haW5pbmcgYWZ0ZXIgcmVhZGluZyByYW5nZS5cIik7XG4gIH1cbiAgcmV0dXJuIHJlc3VsdDtcbn1cblxuLyoqXG4gKiBSZWFkIGEgcmFuZ2Ugb2YgYnl0ZXMgc3luY2hyb25vdXNseSBmcm9tIGEgZmlsZSBvciBvdGhlciByZXNvdXJjZSB0aGF0IGlzXG4gKiByZWFkYWJsZSBhbmQgc2Vla2FibGUuICBUaGUgcmFuZ2Ugc3RhcnQgYW5kIGVuZCBhcmUgaW5jbHVzaXZlIG9mIHRoZSBieXRlc1xuICogd2l0aGluIHRoYXQgcmFuZ2UuXG4gKlxuICogYGBgdHNcbiAqIGltcG9ydCB7IGFzc2VydEVxdWFscyB9IGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL2Fzc2VydC9hc3NlcnRfZXF1YWxzLnRzXCI7XG4gKiBpbXBvcnQgeyByZWFkUmFuZ2VTeW5jIH0gZnJvbSBcImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAkU1REX1ZFUlNJT04vaW8vcmVhZF9yYW5nZS50c1wiO1xuICpcbiAqIC8vIFJlYWQgdGhlIGZpcnN0IDEwIGJ5dGVzIG9mIGEgZmlsZVxuICogY29uc3QgZmlsZSA9IERlbm8ub3BlblN5bmMoXCJleGFtcGxlLnR4dFwiLCB7IHJlYWQ6IHRydWUgfSk7XG4gKiBjb25zdCBieXRlcyA9IHJlYWRSYW5nZVN5bmMoZmlsZSwgeyBzdGFydDogMCwgZW5kOiA5IH0pO1xuICogYXNzZXJ0RXF1YWxzKGJ5dGVzLmxlbmd0aCwgMTApO1xuICogYGBgXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHJlYWRSYW5nZVN5bmMoXG4gIHI6IFJlYWRlclN5bmMgJiBEZW5vLlNlZWtlclN5bmMsXG4gIHJhbmdlOiBCeXRlUmFuZ2UsXG4pOiBVaW50OEFycmF5IHtcbiAgLy8gYnl0ZSByYW5nZXMgYXJlIGluY2x1c2l2ZSwgc28gd2UgaGF2ZSB0byBhZGQgb25lIHRvIHRoZSBlbmRcbiAgbGV0IGxlbmd0aCA9IHJhbmdlLmVuZCAtIHJhbmdlLnN0YXJ0ICsgMTtcbiAgYXNzZXJ0KGxlbmd0aCA+IDAsIFwiSW52YWxpZCBieXRlIHJhbmdlIHdhcyBwYXNzZWQuXCIpO1xuICByLnNlZWtTeW5jKHJhbmdlLnN0YXJ0LCBEZW5vLlNlZWtNb2RlLlN0YXJ0KTtcbiAgY29uc3QgcmVzdWx0ID0gbmV3IFVpbnQ4QXJyYXkobGVuZ3RoKTtcbiAgbGV0IG9mZiA9IDA7XG4gIHdoaWxlIChsZW5ndGgpIHtcbiAgICBjb25zdCBwID0gbmV3IFVpbnQ4QXJyYXkoTWF0aC5taW4obGVuZ3RoLCBERUZBVUxUX0JVRkZFUl9TSVpFKSk7XG4gICAgY29uc3QgbnJlYWQgPSByLnJlYWRTeW5jKHApO1xuICAgIGFzc2VydChucmVhZCAhPT0gbnVsbCwgXCJVbmV4cGVjdGVkIEVPRiByZWFjaCB3aGlsZSByZWFkaW5nIGEgcmFuZ2UuXCIpO1xuICAgIGFzc2VydChucmVhZCA+IDAsIFwiVW5leHBlY3RlZCByZWFkIG9mIDAgYnl0ZXMgd2hpbGUgcmVhZGluZyBhIHJhbmdlLlwiKTtcbiAgICBjb3B5Qnl0ZXMocCwgcmVzdWx0LCBvZmYpO1xuICAgIG9mZiArPSBucmVhZDtcbiAgICBsZW5ndGggLT0gbnJlYWQ7XG4gICAgYXNzZXJ0KGxlbmd0aCA+PSAwLCBcIlVuZXhwZWN0ZWQgbGVuZ3RoIHJlbWFpbmluZyBhZnRlciByZWFkaW5nIHJhbmdlLlwiKTtcbiAgfVxuICByZXR1cm4gcmVzdWx0O1xufVxuIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBLDBFQUEwRTtBQUUxRSxTQUFTLFFBQVEsU0FBUyxRQUFRLG1CQUFtQjtBQUNyRCxTQUFTLE1BQU0sUUFBUSxzQkFBc0I7QUFHN0MsTUFBTSxzQkFBc0IsS0FBSztBQWFqQzs7Ozs7Ozs7Ozs7Ozs7OztDQWdCQyxHQUNELE9BQU8sZUFBZSxVQUNwQixDQUF1QixFQUN2QixLQUFnQjtFQUVoQiw4REFBOEQ7RUFDOUQsSUFBSSxTQUFTLE1BQU0sR0FBRyxHQUFHLE1BQU0sS0FBSyxHQUFHO0VBQ3ZDLE9BQU8sU0FBUyxHQUFHO0VBQ25CLE1BQU0sRUFBRSxJQUFJLENBQUMsTUFBTSxLQUFLLEVBQUUsS0FBSyxRQUFRLENBQUMsS0FBSztFQUM3QyxNQUFNLFNBQVMsSUFBSSxXQUFXO0VBQzlCLElBQUksTUFBTTtFQUNWLE1BQU8sT0FBUTtJQUNiLE1BQU0sSUFBSSxJQUFJLFdBQVcsS0FBSyxHQUFHLENBQUMsUUFBUTtJQUMxQyxNQUFNLFFBQVEsTUFBTSxFQUFFLElBQUksQ0FBQztJQUMzQixPQUFPLFVBQVUsTUFBTTtJQUN2QixPQUFPLFFBQVEsR0FBRztJQUNsQixVQUFVLEdBQUcsUUFBUTtJQUNyQixPQUFPO0lBQ1AsVUFBVTtJQUNWLE9BQU8sVUFBVSxHQUFHO0VBQ3RCO0VBQ0EsT0FBTztBQUNUO0FBRUE7Ozs7Ozs7Ozs7Ozs7Ozs7Q0FnQkMsR0FDRCxPQUFPLFNBQVMsY0FDZCxDQUErQixFQUMvQixLQUFnQjtFQUVoQiw4REFBOEQ7RUFDOUQsSUFBSSxTQUFTLE1BQU0sR0FBRyxHQUFHLE1BQU0sS0FBSyxHQUFHO0VBQ3ZDLE9BQU8sU0FBUyxHQUFHO0VBQ25CLEVBQUUsUUFBUSxDQUFDLE1BQU0sS0FBSyxFQUFFLEtBQUssUUFBUSxDQUFDLEtBQUs7RUFDM0MsTUFBTSxTQUFTLElBQUksV0FBVztFQUM5QixJQUFJLE1BQU07RUFDVixNQUFPLE9BQVE7SUFDYixNQUFNLElBQUksSUFBSSxXQUFXLEtBQUssR0FBRyxDQUFDLFFBQVE7SUFDMUMsTUFBTSxRQUFRLEVBQUUsUUFBUSxDQUFDO0lBQ3pCLE9BQU8sVUFBVSxNQUFNO0lBQ3ZCLE9BQU8sUUFBUSxHQUFHO0lBQ2xCLFVBQVUsR0FBRyxRQUFRO0lBQ3JCLE9BQU87SUFDUCxVQUFVO0lBQ1YsT0FBTyxVQUFVLEdBQUc7RUFDdEI7RUFDQSxPQUFPO0FBQ1QifQ==
// denoCacheMetadata=3144517395224850478,12100324066726032704