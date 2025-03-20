// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { writeAll } from "./write_all.ts";
import { isCloser } from "./_common.ts";
/**
 * Create a {@linkcode WritableStream} from a {@linkcode Writer}.
 *
 * @example
 * ```ts
 * import { toWritableStream } from "https://deno.land/std@$STD_VERSION/io/to_writable_stream.ts";
 *
 * const file = await Deno.open("./file.txt", { create: true, write: true });
 * await ReadableStream.from("Hello World")
 *   .pipeThrough(new TextEncoderStream())
 *   .pipeTo(toWritableStream(file));
 * ```
 */ export function toWritableStream(writer, { autoClose = true } = {}) {
  return new WritableStream({
    async write (chunk, controller) {
      try {
        await writeAll(writer, chunk);
      } catch (e) {
        controller.error(e);
        if (isCloser(writer) && autoClose) {
          writer.close();
        }
      }
    },
    close () {
      if (isCloser(writer) && autoClose) {
        writer.close();
      }
    },
    abort () {
      if (isCloser(writer) && autoClose) {
        writer.close();
      }
    }
  });
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3RvX3dyaXRhYmxlX3N0cmVhbS50cyJdLCJzb3VyY2VzQ29udGVudCI6WyIvLyBDb3B5cmlnaHQgMjAxOC0yMDI0IHRoZSBEZW5vIGF1dGhvcnMuIEFsbCByaWdodHMgcmVzZXJ2ZWQuIE1JVCBsaWNlbnNlLlxuLy8gVGhpcyBtb2R1bGUgaXMgYnJvd3NlciBjb21wYXRpYmxlLlxuXG5pbXBvcnQgeyB3cml0ZUFsbCB9IGZyb20gXCIuL3dyaXRlX2FsbC50c1wiO1xuaW1wb3J0IHR5cGUgeyBXcml0ZXIgfSBmcm9tIFwiLi90eXBlcy50c1wiO1xuaW1wb3J0IHsgaXNDbG9zZXIgfSBmcm9tIFwiLi9fY29tbW9uLnRzXCI7XG5cbi8qKiBPcHRpb25zIGZvciB7QGxpbmtjb2RlIHRvV3JpdGFibGVTdHJlYW19LiAqL1xuZXhwb3J0IGludGVyZmFjZSB0b1dyaXRhYmxlU3RyZWFtT3B0aW9ucyB7XG4gIC8qKlxuICAgKiBJZiB0aGUgYHdyaXRlcmAgaXMgYWxzbyBhIGBDbG9zZXJgLCBhdXRvbWF0aWNhbGx5IGNsb3NlIHRoZSBgd3JpdGVyYFxuICAgKiB3aGVuIHRoZSBzdHJlYW0gaXMgY2xvc2VkLCBhYm9ydGVkLCBvciBhIHdyaXRlIGVycm9yIG9jY3Vycy5cbiAgICpcbiAgICogQGRlZmF1bHQge3RydWV9XG4gICAqL1xuICBhdXRvQ2xvc2U/OiBib29sZWFuO1xufVxuXG4vKipcbiAqIENyZWF0ZSBhIHtAbGlua2NvZGUgV3JpdGFibGVTdHJlYW19IGZyb20gYSB7QGxpbmtjb2RlIFdyaXRlcn0uXG4gKlxuICogQGV4YW1wbGVcbiAqIGBgYHRzXG4gKiBpbXBvcnQgeyB0b1dyaXRhYmxlU3RyZWFtIH0gZnJvbSBcImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAkU1REX1ZFUlNJT04vaW8vdG9fd3JpdGFibGVfc3RyZWFtLnRzXCI7XG4gKlxuICogY29uc3QgZmlsZSA9IGF3YWl0IERlbm8ub3BlbihcIi4vZmlsZS50eHRcIiwgeyBjcmVhdGU6IHRydWUsIHdyaXRlOiB0cnVlIH0pO1xuICogYXdhaXQgUmVhZGFibGVTdHJlYW0uZnJvbShcIkhlbGxvIFdvcmxkXCIpXG4gKiAgIC5waXBlVGhyb3VnaChuZXcgVGV4dEVuY29kZXJTdHJlYW0oKSlcbiAqICAgLnBpcGVUbyh0b1dyaXRhYmxlU3RyZWFtKGZpbGUpKTtcbiAqIGBgYFxuICovXG5leHBvcnQgZnVuY3Rpb24gdG9Xcml0YWJsZVN0cmVhbShcbiAgd3JpdGVyOiBXcml0ZXIsXG4gIHsgYXV0b0Nsb3NlID0gdHJ1ZSB9OiB0b1dyaXRhYmxlU3RyZWFtT3B0aW9ucyA9IHt9LFxuKTogV3JpdGFibGVTdHJlYW08VWludDhBcnJheT4ge1xuICByZXR1cm4gbmV3IFdyaXRhYmxlU3RyZWFtKHtcbiAgICBhc3luYyB3cml0ZShjaHVuaywgY29udHJvbGxlcikge1xuICAgICAgdHJ5IHtcbiAgICAgICAgYXdhaXQgd3JpdGVBbGwod3JpdGVyLCBjaHVuayk7XG4gICAgICB9IGNhdGNoIChlKSB7XG4gICAgICAgIGNvbnRyb2xsZXIuZXJyb3IoZSk7XG4gICAgICAgIGlmIChpc0Nsb3Nlcih3cml0ZXIpICYmIGF1dG9DbG9zZSkge1xuICAgICAgICAgIHdyaXRlci5jbG9zZSgpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSxcbiAgICBjbG9zZSgpIHtcbiAgICAgIGlmIChpc0Nsb3Nlcih3cml0ZXIpICYmIGF1dG9DbG9zZSkge1xuICAgICAgICB3cml0ZXIuY2xvc2UoKTtcbiAgICAgIH1cbiAgICB9LFxuICAgIGFib3J0KCkge1xuICAgICAgaWYgKGlzQ2xvc2VyKHdyaXRlcikgJiYgYXV0b0Nsb3NlKSB7XG4gICAgICAgIHdyaXRlci5jbG9zZSgpO1xuICAgICAgfVxuICAgIH0sXG4gIH0pO1xufVxuIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBLDBFQUEwRTtBQUMxRSxxQ0FBcUM7QUFFckMsU0FBUyxRQUFRLFFBQVEsaUJBQWlCO0FBRTFDLFNBQVMsUUFBUSxRQUFRLGVBQWU7QUFheEM7Ozs7Ozs7Ozs7OztDQVlDLEdBQ0QsT0FBTyxTQUFTLGlCQUNkLE1BQWMsRUFDZCxFQUFFLFlBQVksSUFBSSxFQUEyQixHQUFHLENBQUMsQ0FBQztFQUVsRCxPQUFPLElBQUksZUFBZTtJQUN4QixNQUFNLE9BQU0sS0FBSyxFQUFFLFVBQVU7TUFDM0IsSUFBSTtRQUNGLE1BQU0sU0FBUyxRQUFRO01BQ3pCLEVBQUUsT0FBTyxHQUFHO1FBQ1YsV0FBVyxLQUFLLENBQUM7UUFDakIsSUFBSSxTQUFTLFdBQVcsV0FBVztVQUNqQyxPQUFPLEtBQUs7UUFDZDtNQUNGO0lBQ0Y7SUFDQTtNQUNFLElBQUksU0FBUyxXQUFXLFdBQVc7UUFDakMsT0FBTyxLQUFLO01BQ2Q7SUFDRjtJQUNBO01BQ0UsSUFBSSxTQUFTLFdBQVcsV0FBVztRQUNqQyxPQUFPLEtBQUs7TUFDZDtJQUNGO0VBQ0Y7QUFDRiJ9
// denoCacheMetadata=1017042547822538619,1655186124574279657