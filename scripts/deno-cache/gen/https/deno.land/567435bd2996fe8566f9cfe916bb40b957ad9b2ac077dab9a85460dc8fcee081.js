// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
import { BufReader } from "./buf_reader.ts";
import { concat } from "../bytes/concat.ts";
/**
 * Read strings line-by-line from a Reader.
 *
 *  @example
 * ```ts
 * import { readLines } from "https://deno.land/std@$STD_VERSION/io/read_lines.ts";
 * import * as path from "https://deno.land/std@$STD_VERSION/path/mod.ts";
 *
 * const filename = path.join(Deno.cwd(), "std/io/README.md");
 * let fileReader = await Deno.open(filename);
 *
 * for await (let line of readLines(fileReader)) {
 *   console.log(line);
 * }
 * ```
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export async function* readLines(reader, decoderOpts) {
  const bufReader = new BufReader(reader);
  let chunks = [];
  const decoder = new TextDecoder(decoderOpts?.encoding, decoderOpts);
  while(true){
    const res = await bufReader.readLine();
    if (!res) {
      if (chunks.length > 0) {
        yield decoder.decode(concat(chunks));
      }
      break;
    }
    chunks.push(res.line);
    if (!res.more) {
      yield decoder.decode(concat(chunks));
      chunks = [];
    }
  }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3JlYWRfbGluZXMudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cbi8vIFRoaXMgbW9kdWxlIGlzIGJyb3dzZXIgY29tcGF0aWJsZS5cblxuaW1wb3J0IHR5cGUgeyBSZWFkZXIgfSBmcm9tIFwiLi90eXBlcy50c1wiO1xuaW1wb3J0IHsgQnVmUmVhZGVyIH0gZnJvbSBcIi4vYnVmX3JlYWRlci50c1wiO1xuaW1wb3J0IHsgY29uY2F0IH0gZnJvbSBcIi4uL2J5dGVzL2NvbmNhdC50c1wiO1xuXG4vKipcbiAqIFJlYWQgc3RyaW5ncyBsaW5lLWJ5LWxpbmUgZnJvbSBhIFJlYWRlci5cbiAqXG4gKiAgQGV4YW1wbGVcbiAqIGBgYHRzXG4gKiBpbXBvcnQgeyByZWFkTGluZXMgfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9pby9yZWFkX2xpbmVzLnRzXCI7XG4gKiBpbXBvcnQgKiBhcyBwYXRoIGZyb20gXCJodHRwczovL2Rlbm8ubGFuZC9zdGRAJFNURF9WRVJTSU9OL3BhdGgvbW9kLnRzXCI7XG4gKlxuICogY29uc3QgZmlsZW5hbWUgPSBwYXRoLmpvaW4oRGVuby5jd2QoKSwgXCJzdGQvaW8vUkVBRE1FLm1kXCIpO1xuICogbGV0IGZpbGVSZWFkZXIgPSBhd2FpdCBEZW5vLm9wZW4oZmlsZW5hbWUpO1xuICpcbiAqIGZvciBhd2FpdCAobGV0IGxpbmUgb2YgcmVhZExpbmVzKGZpbGVSZWFkZXIpKSB7XG4gKiAgIGNvbnNvbGUubG9nKGxpbmUpO1xuICogfVxuICogYGBgXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGFzeW5jIGZ1bmN0aW9uKiByZWFkTGluZXMoXG4gIHJlYWRlcjogUmVhZGVyLFxuICBkZWNvZGVyT3B0cz86IHtcbiAgICBlbmNvZGluZz86IHN0cmluZztcbiAgICBmYXRhbD86IGJvb2xlYW47XG4gICAgaWdub3JlQk9NPzogYm9vbGVhbjtcbiAgfSxcbik6IEFzeW5jSXRlcmFibGVJdGVyYXRvcjxzdHJpbmc+IHtcbiAgY29uc3QgYnVmUmVhZGVyID0gbmV3IEJ1ZlJlYWRlcihyZWFkZXIpO1xuICBsZXQgY2h1bmtzOiBVaW50OEFycmF5W10gPSBbXTtcbiAgY29uc3QgZGVjb2RlciA9IG5ldyBUZXh0RGVjb2RlcihkZWNvZGVyT3B0cz8uZW5jb2RpbmcsIGRlY29kZXJPcHRzKTtcbiAgd2hpbGUgKHRydWUpIHtcbiAgICBjb25zdCByZXMgPSBhd2FpdCBidWZSZWFkZXIucmVhZExpbmUoKTtcbiAgICBpZiAoIXJlcykge1xuICAgICAgaWYgKGNodW5rcy5sZW5ndGggPiAwKSB7XG4gICAgICAgIHlpZWxkIGRlY29kZXIuZGVjb2RlKGNvbmNhdChjaHVua3MpKTtcbiAgICAgIH1cbiAgICAgIGJyZWFrO1xuICAgIH1cbiAgICBjaHVua3MucHVzaChyZXMubGluZSk7XG4gICAgaWYgKCFyZXMubW9yZSkge1xuICAgICAgeWllbGQgZGVjb2Rlci5kZWNvZGUoY29uY2F0KGNodW5rcykpO1xuICAgICAgY2h1bmtzID0gW107XG4gICAgfVxuICB9XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUdyQyxTQUFTLFNBQVMsUUFBUSxrQkFBa0I7QUFDNUMsU0FBUyxNQUFNLFFBQVEscUJBQXFCO0FBRTVDOzs7Ozs7Ozs7Ozs7Ozs7OztDQWlCQyxHQUNELE9BQU8sZ0JBQWdCLFVBQ3JCLE1BQWMsRUFDZCxXQUlDO0VBRUQsTUFBTSxZQUFZLElBQUksVUFBVTtFQUNoQyxJQUFJLFNBQXVCLEVBQUU7RUFDN0IsTUFBTSxVQUFVLElBQUksWUFBWSxhQUFhLFVBQVU7RUFDdkQsTUFBTyxLQUFNO0lBQ1gsTUFBTSxNQUFNLE1BQU0sVUFBVSxRQUFRO0lBQ3BDLElBQUksQ0FBQyxLQUFLO01BQ1IsSUFBSSxPQUFPLE1BQU0sR0FBRyxHQUFHO1FBQ3JCLE1BQU0sUUFBUSxNQUFNLENBQUMsT0FBTztNQUM5QjtNQUNBO0lBQ0Y7SUFDQSxPQUFPLElBQUksQ0FBQyxJQUFJLElBQUk7SUFDcEIsSUFBSSxDQUFDLElBQUksSUFBSSxFQUFFO01BQ2IsTUFBTSxRQUFRLE1BQU0sQ0FBQyxPQUFPO01BQzVCLFNBQVMsRUFBRTtJQUNiO0VBQ0Y7QUFDRiJ9
// denoCacheMetadata=14712675731470571886,10820485161135552756