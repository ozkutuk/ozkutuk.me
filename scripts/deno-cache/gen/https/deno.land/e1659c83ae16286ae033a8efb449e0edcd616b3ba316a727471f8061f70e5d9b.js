// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { Buffer } from "./buffer.ts";
import { writeAll } from "./write_all.ts";
/**
 * Create a {@linkcode Reader} from a {@linkcode ReadableStreamDefaultReader}.
 *
 * @example
 * ```ts
 * import { copy } from "https://deno.land/std@$STD_VERSION/io/copy.ts";
 * import { readerFromStreamReader } from "https://deno.land/std@$STD_VERSION/io/reader_from_stream_reader.ts";
 *
 * const res = await fetch("https://deno.land");
 * using file = await Deno.open("./deno.land.html", { create: true, write: true });
 *
 * const reader = readerFromStreamReader(res.body!.getReader());
 * await copy(reader, file);
 * ```
 */ export function readerFromStreamReader(streamReader) {
  const buffer = new Buffer();
  return {
    async read (p) {
      if (buffer.empty()) {
        const res = await streamReader.read();
        if (res.done) {
          return null; // EOF
        }
        await writeAll(buffer, res.value);
      }
      return buffer.read(p);
    }
  };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3JlYWRlcl9mcm9tX3N0cmVhbV9yZWFkZXIudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cbi8vIFRoaXMgbW9kdWxlIGlzIGJyb3dzZXIgY29tcGF0aWJsZS5cblxuaW1wb3J0IHsgQnVmZmVyIH0gZnJvbSBcIi4vYnVmZmVyLnRzXCI7XG5pbXBvcnQgeyB3cml0ZUFsbCB9IGZyb20gXCIuL3dyaXRlX2FsbC50c1wiO1xuaW1wb3J0IHR5cGUgeyBSZWFkZXIgfSBmcm9tIFwiLi90eXBlcy50c1wiO1xuXG4vKipcbiAqIENyZWF0ZSBhIHtAbGlua2NvZGUgUmVhZGVyfSBmcm9tIGEge0BsaW5rY29kZSBSZWFkYWJsZVN0cmVhbURlZmF1bHRSZWFkZXJ9LlxuICpcbiAqIEBleGFtcGxlXG4gKiBgYGB0c1xuICogaW1wb3J0IHsgY29weSB9IGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL2lvL2NvcHkudHNcIjtcbiAqIGltcG9ydCB7IHJlYWRlckZyb21TdHJlYW1SZWFkZXIgfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby9yZWFkZXJfZnJvbV9zdHJlYW1fcmVhZGVyLnRzXCI7XG4gKlxuICogY29uc3QgcmVzID0gYXdhaXQgZmV0Y2goXCJodHRwczovL2Rlbm8ubGFuZFwiKTtcbiAqIHVzaW5nIGZpbGUgPSBhd2FpdCBEZW5vLm9wZW4oXCIuL2Rlbm8ubGFuZC5odG1sXCIsIHsgY3JlYXRlOiB0cnVlLCB3cml0ZTogdHJ1ZSB9KTtcbiAqXG4gKiBjb25zdCByZWFkZXIgPSByZWFkZXJGcm9tU3RyZWFtUmVhZGVyKHJlcy5ib2R5IS5nZXRSZWFkZXIoKSk7XG4gKiBhd2FpdCBjb3B5KHJlYWRlciwgZmlsZSk7XG4gKiBgYGBcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHJlYWRlckZyb21TdHJlYW1SZWFkZXIoXG4gIHN0cmVhbVJlYWRlcjogUmVhZGFibGVTdHJlYW1EZWZhdWx0UmVhZGVyPFVpbnQ4QXJyYXk+LFxuKTogUmVhZGVyIHtcbiAgY29uc3QgYnVmZmVyID0gbmV3IEJ1ZmZlcigpO1xuXG4gIHJldHVybiB7XG4gICAgYXN5bmMgcmVhZChwOiBVaW50OEFycmF5KTogUHJvbWlzZTxudW1iZXIgfCBudWxsPiB7XG4gICAgICBpZiAoYnVmZmVyLmVtcHR5KCkpIHtcbiAgICAgICAgY29uc3QgcmVzID0gYXdhaXQgc3RyZWFtUmVhZGVyLnJlYWQoKTtcbiAgICAgICAgaWYgKHJlcy5kb25lKSB7XG4gICAgICAgICAgcmV0dXJuIG51bGw7IC8vIEVPRlxuICAgICAgICB9XG5cbiAgICAgICAgYXdhaXQgd3JpdGVBbGwoYnVmZmVyLCByZXMudmFsdWUpO1xuICAgICAgfVxuXG4gICAgICByZXR1cm4gYnVmZmVyLnJlYWQocCk7XG4gICAgfSxcbiAgfTtcbn1cbiJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSwwRUFBMEU7QUFDMUUscUNBQXFDO0FBRXJDLFNBQVMsTUFBTSxRQUFRLGNBQWM7QUFDckMsU0FBUyxRQUFRLFFBQVEsaUJBQWlCO0FBRzFDOzs7Ozs7Ozs7Ozs7OztDQWNDLEdBQ0QsT0FBTyxTQUFTLHVCQUNkLFlBQXFEO0VBRXJELE1BQU0sU0FBUyxJQUFJO0VBRW5CLE9BQU87SUFDTCxNQUFNLE1BQUssQ0FBYTtNQUN0QixJQUFJLE9BQU8sS0FBSyxJQUFJO1FBQ2xCLE1BQU0sTUFBTSxNQUFNLGFBQWEsSUFBSTtRQUNuQyxJQUFJLElBQUksSUFBSSxFQUFFO1VBQ1osT0FBTyxNQUFNLE1BQU07UUFDckI7UUFFQSxNQUFNLFNBQVMsUUFBUSxJQUFJLEtBQUs7TUFDbEM7TUFFQSxPQUFPLE9BQU8sSUFBSSxDQUFDO0lBQ3JCO0VBQ0Y7QUFDRiJ9
// denoCacheMetadata=3060467258279802883,9531553271954744959