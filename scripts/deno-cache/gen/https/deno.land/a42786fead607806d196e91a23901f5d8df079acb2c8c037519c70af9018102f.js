// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { concat } from "../bytes/concat.ts";
import { DEFAULT_CHUNK_SIZE } from "./_constants.ts";
/**
 * Read {@linkcode Reader} `r` until EOF (`null`) and resolve to the content as
 * {@linkcode Uint8Array}.
 *
 * @example
 * ```ts
 * import { readAll } from "https://deno.land/std@$STD_VERSION/io/read_all.ts";
 *
 * // Example from stdin
 * const stdinContent = await readAll(Deno.stdin);
 *
 * // Example from file
 * using file = await Deno.open("my_file.txt", {read: true});
 * const myFileContent = await readAll(file);
 * ```
 */ export async function readAll(reader) {
  const chunks = [];
  while(true){
    let chunk = new Uint8Array(DEFAULT_CHUNK_SIZE);
    const n = await reader.read(chunk);
    if (n === null) {
      break;
    }
    if (n < DEFAULT_CHUNK_SIZE) {
      chunk = chunk.subarray(0, n);
    }
    chunks.push(chunk);
  }
  return concat(chunks);
}
/**
 * Synchronously reads {@linkcode ReaderSync} `r` until EOF (`null`) and returns
 * the content as {@linkcode Uint8Array}.
 *
 * @example
 * ```ts
 * import { readAllSync } from "https://deno.land/std@$STD_VERSION/io/read_all.ts";
 *
 * // Example from stdin
 * const stdinContent = readAllSync(Deno.stdin);
 *
 * // Example from file
 * using file = Deno.openSync("my_file.txt", {read: true});
 * const myFileContent = readAllSync(file);
 * ```
 */ export function readAllSync(reader) {
  const chunks = [];
  while(true){
    const chunk = new Uint8Array(DEFAULT_CHUNK_SIZE);
    const n = reader.readSync(chunk);
    if (n === null) {
      break;
    }
    if (n < DEFAULT_CHUNK_SIZE) {
      chunks.push(chunk.subarray(0, n));
      break;
    }
    chunks.push(chunk);
  }
  return concat(chunks);
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3JlYWRfYWxsLnRzIl0sInNvdXJjZXNDb250ZW50IjpbIi8vIENvcHlyaWdodCAyMDE4LTIwMjQgdGhlIERlbm8gYXV0aG9ycy4gQWxsIHJpZ2h0cyByZXNlcnZlZC4gTUlUIGxpY2Vuc2UuXG4vLyBUaGlzIG1vZHVsZSBpcyBicm93c2VyIGNvbXBhdGlibGUuXG5cbmltcG9ydCB7IGNvbmNhdCB9IGZyb20gXCIuLi9ieXRlcy9jb25jYXQudHNcIjtcbmltcG9ydCB7IERFRkFVTFRfQ0hVTktfU0laRSB9IGZyb20gXCIuL19jb25zdGFudHMudHNcIjtcbmltcG9ydCB0eXBlIHsgUmVhZGVyLCBSZWFkZXJTeW5jIH0gZnJvbSBcIi4vdHlwZXMudHNcIjtcblxuLyoqXG4gKiBSZWFkIHtAbGlua2NvZGUgUmVhZGVyfSBgcmAgdW50aWwgRU9GIChgbnVsbGApIGFuZCByZXNvbHZlIHRvIHRoZSBjb250ZW50IGFzXG4gKiB7QGxpbmtjb2RlIFVpbnQ4QXJyYXl9LlxuICpcbiAqIEBleGFtcGxlXG4gKiBgYGB0c1xuICogaW1wb3J0IHsgcmVhZEFsbCB9IGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL2lvL3JlYWRfYWxsLnRzXCI7XG4gKlxuICogLy8gRXhhbXBsZSBmcm9tIHN0ZGluXG4gKiBjb25zdCBzdGRpbkNvbnRlbnQgPSBhd2FpdCByZWFkQWxsKERlbm8uc3RkaW4pO1xuICpcbiAqIC8vIEV4YW1wbGUgZnJvbSBmaWxlXG4gKiB1c2luZyBmaWxlID0gYXdhaXQgRGVuby5vcGVuKFwibXlfZmlsZS50eHRcIiwge3JlYWQ6IHRydWV9KTtcbiAqIGNvbnN0IG15RmlsZUNvbnRlbnQgPSBhd2FpdCByZWFkQWxsKGZpbGUpO1xuICogYGBgXG4gKi9cbmV4cG9ydCBhc3luYyBmdW5jdGlvbiByZWFkQWxsKHJlYWRlcjogUmVhZGVyKTogUHJvbWlzZTxVaW50OEFycmF5PiB7XG4gIGNvbnN0IGNodW5rczogVWludDhBcnJheVtdID0gW107XG4gIHdoaWxlICh0cnVlKSB7XG4gICAgbGV0IGNodW5rID0gbmV3IFVpbnQ4QXJyYXkoREVGQVVMVF9DSFVOS19TSVpFKTtcbiAgICBjb25zdCBuID0gYXdhaXQgcmVhZGVyLnJlYWQoY2h1bmspO1xuICAgIGlmIChuID09PSBudWxsKSB7XG4gICAgICBicmVhaztcbiAgICB9XG4gICAgaWYgKG4gPCBERUZBVUxUX0NIVU5LX1NJWkUpIHtcbiAgICAgIGNodW5rID0gY2h1bmsuc3ViYXJyYXkoMCwgbik7XG4gICAgfVxuICAgIGNodW5rcy5wdXNoKGNodW5rKTtcbiAgfVxuICByZXR1cm4gY29uY2F0KGNodW5rcyk7XG59XG5cbi8qKlxuICogU3luY2hyb25vdXNseSByZWFkcyB7QGxpbmtjb2RlIFJlYWRlclN5bmN9IGByYCB1bnRpbCBFT0YgKGBudWxsYCkgYW5kIHJldHVybnNcbiAqIHRoZSBjb250ZW50IGFzIHtAbGlua2NvZGUgVWludDhBcnJheX0uXG4gKlxuICogQGV4YW1wbGVcbiAqIGBgYHRzXG4gKiBpbXBvcnQgeyByZWFkQWxsU3luYyB9IGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL2lvL3JlYWRfYWxsLnRzXCI7XG4gKlxuICogLy8gRXhhbXBsZSBmcm9tIHN0ZGluXG4gKiBjb25zdCBzdGRpbkNvbnRlbnQgPSByZWFkQWxsU3luYyhEZW5vLnN0ZGluKTtcbiAqXG4gKiAvLyBFeGFtcGxlIGZyb20gZmlsZVxuICogdXNpbmcgZmlsZSA9IERlbm8ub3BlblN5bmMoXCJteV9maWxlLnR4dFwiLCB7cmVhZDogdHJ1ZX0pO1xuICogY29uc3QgbXlGaWxlQ29udGVudCA9IHJlYWRBbGxTeW5jKGZpbGUpO1xuICogYGBgXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiByZWFkQWxsU3luYyhyZWFkZXI6IFJlYWRlclN5bmMpOiBVaW50OEFycmF5IHtcbiAgY29uc3QgY2h1bmtzOiBVaW50OEFycmF5W10gPSBbXTtcbiAgd2hpbGUgKHRydWUpIHtcbiAgICBjb25zdCBjaHVuayA9IG5ldyBVaW50OEFycmF5KERFRkFVTFRfQ0hVTktfU0laRSk7XG4gICAgY29uc3QgbiA9IHJlYWRlci5yZWFkU3luYyhjaHVuayk7XG4gICAgaWYgKG4gPT09IG51bGwpIHtcbiAgICAgIGJyZWFrO1xuICAgIH1cbiAgICBpZiAobiA8IERFRkFVTFRfQ0hVTktfU0laRSkge1xuICAgICAgY2h1bmtzLnB1c2goY2h1bmsuc3ViYXJyYXkoMCwgbikpO1xuICAgICAgYnJlYWs7XG4gICAgfVxuICAgIGNodW5rcy5wdXNoKGNodW5rKTtcbiAgfVxuICByZXR1cm4gY29uY2F0KGNodW5rcyk7XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQyxTQUFTLE1BQU0sUUFBUSxxQkFBcUI7QUFDNUMsU0FBUyxrQkFBa0IsUUFBUSxrQkFBa0I7QUFHckQ7Ozs7Ozs7Ozs7Ozs7OztDQWVDLEdBQ0QsT0FBTyxlQUFlLFFBQVEsTUFBYztFQUMxQyxNQUFNLFNBQXVCLEVBQUU7RUFDL0IsTUFBTyxLQUFNO0lBQ1gsSUFBSSxRQUFRLElBQUksV0FBVztJQUMzQixNQUFNLElBQUksTUFBTSxPQUFPLElBQUksQ0FBQztJQUM1QixJQUFJLE1BQU0sTUFBTTtNQUNkO0lBQ0Y7SUFDQSxJQUFJLElBQUksb0JBQW9CO01BQzFCLFFBQVEsTUFBTSxRQUFRLENBQUMsR0FBRztJQUM1QjtJQUNBLE9BQU8sSUFBSSxDQUFDO0VBQ2Q7RUFDQSxPQUFPLE9BQU87QUFDaEI7QUFFQTs7Ozs7Ozs7Ozs7Ozs7O0NBZUMsR0FDRCxPQUFPLFNBQVMsWUFBWSxNQUFrQjtFQUM1QyxNQUFNLFNBQXVCLEVBQUU7RUFDL0IsTUFBTyxLQUFNO0lBQ1gsTUFBTSxRQUFRLElBQUksV0FBVztJQUM3QixNQUFNLElBQUksT0FBTyxRQUFRLENBQUM7SUFDMUIsSUFBSSxNQUFNLE1BQU07TUFDZDtJQUNGO0lBQ0EsSUFBSSxJQUFJLG9CQUFvQjtNQUMxQixPQUFPLElBQUksQ0FBQyxNQUFNLFFBQVEsQ0FBQyxHQUFHO01BQzlCO0lBQ0Y7SUFDQSxPQUFPLElBQUksQ0FBQztFQUNkO0VBQ0EsT0FBTyxPQUFPO0FBQ2hCIn0=
// denoCacheMetadata=10502571897259381208,2005553948406985487