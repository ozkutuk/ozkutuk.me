// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { DEFAULT_CHUNK_SIZE } from "./_constants.ts";
import { isCloser } from "./_common.ts";
/**
 * Create a {@linkcode ReadableStream} of {@linkcode Uint8Array}s from a
 * {@linkcode Reader}.
 *
 * When the pull algorithm is called on the stream, a chunk from the reader
 * will be read.  When `null` is returned from the reader, the stream will be
 * closed along with the reader (if it is also a `Closer`).
 *
 * @example
 * ```ts
 * import { toReadableStream } from "https://deno.land/std@$STD_VERSION/io/to_readable_stream.ts";
 *
 * const file = await Deno.open("./file.txt", { read: true });
 * const fileStream = toReadableStream(file);
 * ```
 */ export function toReadableStream(reader, { autoClose = true, chunkSize = DEFAULT_CHUNK_SIZE, strategy } = {}) {
  return new ReadableStream({
    async pull (controller) {
      const chunk = new Uint8Array(chunkSize);
      try {
        const read = await reader.read(chunk);
        if (read === null) {
          if (isCloser(reader) && autoClose) {
            reader.close();
          }
          controller.close();
          return;
        }
        controller.enqueue(chunk.subarray(0, read));
      } catch (e) {
        controller.error(e);
        if (isCloser(reader)) {
          reader.close();
        }
      }
    },
    cancel () {
      if (isCloser(reader) && autoClose) {
        reader.close();
      }
    }
  }, strategy);
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3RvX3JlYWRhYmxlX3N0cmVhbS50cyJdLCJzb3VyY2VzQ29udGVudCI6WyIvLyBDb3B5cmlnaHQgMjAxOC0yMDI0IHRoZSBEZW5vIGF1dGhvcnMuIEFsbCByaWdodHMgcmVzZXJ2ZWQuIE1JVCBsaWNlbnNlLlxuLy8gVGhpcyBtb2R1bGUgaXMgYnJvd3NlciBjb21wYXRpYmxlLlxuXG5pbXBvcnQgeyBERUZBVUxUX0NIVU5LX1NJWkUgfSBmcm9tIFwiLi9fY29uc3RhbnRzLnRzXCI7XG5pbXBvcnQgeyBpc0Nsb3NlciB9IGZyb20gXCIuL19jb21tb24udHNcIjtcbmltcG9ydCB0eXBlIHsgQ2xvc2VyLCBSZWFkZXIgfSBmcm9tIFwiLi90eXBlcy50c1wiO1xuXG4vKiogT3B0aW9ucyBmb3Ige0BsaW5rY29kZSB0b1JlYWRhYmxlU3RyZWFtfS4gKi9cbmV4cG9ydCBpbnRlcmZhY2UgVG9SZWFkYWJsZVN0cmVhbU9wdGlvbnMge1xuICAvKiogSWYgdGhlIGByZWFkZXJgIGlzIGFsc28gYSBgQ2xvc2VyYCwgYXV0b21hdGljYWxseSBjbG9zZSB0aGUgYHJlYWRlcmBcbiAgICogd2hlbiBgRU9GYCBpcyBlbmNvdW50ZXJlZCwgb3IgYSByZWFkIGVycm9yIG9jY3Vycy5cbiAgICpcbiAgICogQGRlZmF1bHQge3RydWV9XG4gICAqL1xuICBhdXRvQ2xvc2U/OiBib29sZWFuO1xuXG4gIC8qKiBUaGUgc2l6ZSBvZiBjaHVua3MgdG8gYWxsb2NhdGUgdG8gcmVhZCwgdGhlIGRlZmF1bHQgaXMgfjE2S2lCLCB3aGljaCBpc1xuICAgKiB0aGUgbWF4aW11bSBzaXplIHRoYXQgRGVubyBvcGVyYXRpb25zIGNhbiBjdXJyZW50bHkgc3VwcG9ydC4gKi9cbiAgY2h1bmtTaXplPzogbnVtYmVyO1xuXG4gIC8qKiBUaGUgcXVldWluZyBzdHJhdGVneSB0byBjcmVhdGUgdGhlIGBSZWFkYWJsZVN0cmVhbWAgd2l0aC4gKi9cbiAgc3RyYXRlZ3k/OiBRdWV1aW5nU3RyYXRlZ3k8VWludDhBcnJheT47XG59XG5cbi8qKlxuICogQ3JlYXRlIGEge0BsaW5rY29kZSBSZWFkYWJsZVN0cmVhbX0gb2Yge0BsaW5rY29kZSBVaW50OEFycmF5fXMgZnJvbSBhXG4gKiB7QGxpbmtjb2RlIFJlYWRlcn0uXG4gKlxuICogV2hlbiB0aGUgcHVsbCBhbGdvcml0aG0gaXMgY2FsbGVkIG9uIHRoZSBzdHJlYW0sIGEgY2h1bmsgZnJvbSB0aGUgcmVhZGVyXG4gKiB3aWxsIGJlIHJlYWQuICBXaGVuIGBudWxsYCBpcyByZXR1cm5lZCBmcm9tIHRoZSByZWFkZXIsIHRoZSBzdHJlYW0gd2lsbCBiZVxuICogY2xvc2VkIGFsb25nIHdpdGggdGhlIHJlYWRlciAoaWYgaXQgaXMgYWxzbyBhIGBDbG9zZXJgKS5cbiAqXG4gKiBAZXhhbXBsZVxuICogYGBgdHNcbiAqIGltcG9ydCB7IHRvUmVhZGFibGVTdHJlYW0gfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby90b19yZWFkYWJsZV9zdHJlYW0udHNcIjtcbiAqXG4gKiBjb25zdCBmaWxlID0gYXdhaXQgRGVuby5vcGVuKFwiLi9maWxlLnR4dFwiLCB7IHJlYWQ6IHRydWUgfSk7XG4gKiBjb25zdCBmaWxlU3RyZWFtID0gdG9SZWFkYWJsZVN0cmVhbShmaWxlKTtcbiAqIGBgYFxuICovXG5leHBvcnQgZnVuY3Rpb24gdG9SZWFkYWJsZVN0cmVhbShcbiAgcmVhZGVyOiBSZWFkZXIgfCAoUmVhZGVyICYgQ2xvc2VyKSxcbiAge1xuICAgIGF1dG9DbG9zZSA9IHRydWUsXG4gICAgY2h1bmtTaXplID0gREVGQVVMVF9DSFVOS19TSVpFLFxuICAgIHN0cmF0ZWd5LFxuICB9OiBUb1JlYWRhYmxlU3RyZWFtT3B0aW9ucyA9IHt9LFxuKTogUmVhZGFibGVTdHJlYW08VWludDhBcnJheT4ge1xuICByZXR1cm4gbmV3IFJlYWRhYmxlU3RyZWFtKHtcbiAgICBhc3luYyBwdWxsKGNvbnRyb2xsZXIpIHtcbiAgICAgIGNvbnN0IGNodW5rID0gbmV3IFVpbnQ4QXJyYXkoY2h1bmtTaXplKTtcbiAgICAgIHRyeSB7XG4gICAgICAgIGNvbnN0IHJlYWQgPSBhd2FpdCByZWFkZXIucmVhZChjaHVuayk7XG4gICAgICAgIGlmIChyZWFkID09PSBudWxsKSB7XG4gICAgICAgICAgaWYgKGlzQ2xvc2VyKHJlYWRlcikgJiYgYXV0b0Nsb3NlKSB7XG4gICAgICAgICAgICByZWFkZXIuY2xvc2UoKTtcbiAgICAgICAgICB9XG4gICAgICAgICAgY29udHJvbGxlci5jbG9zZSgpO1xuICAgICAgICAgIHJldHVybjtcbiAgICAgICAgfVxuICAgICAgICBjb250cm9sbGVyLmVucXVldWUoY2h1bmsuc3ViYXJyYXkoMCwgcmVhZCkpO1xuICAgICAgfSBjYXRjaCAoZSkge1xuICAgICAgICBjb250cm9sbGVyLmVycm9yKGUpO1xuICAgICAgICBpZiAoaXNDbG9zZXIocmVhZGVyKSkge1xuICAgICAgICAgIHJlYWRlci5jbG9zZSgpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSxcbiAgICBjYW5jZWwoKSB7XG4gICAgICBpZiAoaXNDbG9zZXIocmVhZGVyKSAmJiBhdXRvQ2xvc2UpIHtcbiAgICAgICAgcmVhZGVyLmNsb3NlKCk7XG4gICAgICB9XG4gICAgfSxcbiAgfSwgc3RyYXRlZ3kpO1xufVxuIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBLDBFQUEwRTtBQUMxRSxxQ0FBcUM7QUFFckMsU0FBUyxrQkFBa0IsUUFBUSxrQkFBa0I7QUFDckQsU0FBUyxRQUFRLFFBQVEsZUFBZTtBQW9CeEM7Ozs7Ozs7Ozs7Ozs7OztDQWVDLEdBQ0QsT0FBTyxTQUFTLGlCQUNkLE1BQWtDLEVBQ2xDLEVBQ0UsWUFBWSxJQUFJLEVBQ2hCLFlBQVksa0JBQWtCLEVBQzlCLFFBQVEsRUFDZ0IsR0FBRyxDQUFDLENBQUM7RUFFL0IsT0FBTyxJQUFJLGVBQWU7SUFDeEIsTUFBTSxNQUFLLFVBQVU7TUFDbkIsTUFBTSxRQUFRLElBQUksV0FBVztNQUM3QixJQUFJO1FBQ0YsTUFBTSxPQUFPLE1BQU0sT0FBTyxJQUFJLENBQUM7UUFDL0IsSUFBSSxTQUFTLE1BQU07VUFDakIsSUFBSSxTQUFTLFdBQVcsV0FBVztZQUNqQyxPQUFPLEtBQUs7VUFDZDtVQUNBLFdBQVcsS0FBSztVQUNoQjtRQUNGO1FBQ0EsV0FBVyxPQUFPLENBQUMsTUFBTSxRQUFRLENBQUMsR0FBRztNQUN2QyxFQUFFLE9BQU8sR0FBRztRQUNWLFdBQVcsS0FBSyxDQUFDO1FBQ2pCLElBQUksU0FBUyxTQUFTO1VBQ3BCLE9BQU8sS0FBSztRQUNkO01BQ0Y7SUFDRjtJQUNBO01BQ0UsSUFBSSxTQUFTLFdBQVcsV0FBVztRQUNqQyxPQUFPLEtBQUs7TUFDZDtJQUNGO0VBQ0YsR0FBRztBQUNMIn0=
// denoCacheMetadata=15360595176218314378,3546137048983569901