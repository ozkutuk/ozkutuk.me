// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
import { readShort } from "./read_short.ts";
/**
 * Read big endian 32bit integer from BufReader
 * @param buf
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export async function readInt(buf) {
  const high = await readShort(buf);
  if (high === null) return null;
  const low = await readShort(buf);
  if (low === null) throw new Deno.errors.UnexpectedEof();
  return high << 16 | low;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3JlYWRfaW50LnRzIl0sInNvdXJjZXNDb250ZW50IjpbIi8vIENvcHlyaWdodCAyMDE4LTIwMjQgdGhlIERlbm8gYXV0aG9ycy4gQWxsIHJpZ2h0cyByZXNlcnZlZC4gTUlUIGxpY2Vuc2UuXG5cbmltcG9ydCB0eXBlIHsgQnVmUmVhZGVyIH0gZnJvbSBcIi4vYnVmX3JlYWRlci50c1wiO1xuaW1wb3J0IHsgcmVhZFNob3J0IH0gZnJvbSBcIi4vcmVhZF9zaG9ydC50c1wiO1xuXG4vKipcbiAqIFJlYWQgYmlnIGVuZGlhbiAzMmJpdCBpbnRlZ2VyIGZyb20gQnVmUmVhZGVyXG4gKiBAcGFyYW0gYnVmXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGFzeW5jIGZ1bmN0aW9uIHJlYWRJbnQoYnVmOiBCdWZSZWFkZXIpOiBQcm9taXNlPG51bWJlciB8IG51bGw+IHtcbiAgY29uc3QgaGlnaCA9IGF3YWl0IHJlYWRTaG9ydChidWYpO1xuICBpZiAoaGlnaCA9PT0gbnVsbCkgcmV0dXJuIG51bGw7XG4gIGNvbnN0IGxvdyA9IGF3YWl0IHJlYWRTaG9ydChidWYpO1xuICBpZiAobG93ID09PSBudWxsKSB0aHJvdyBuZXcgRGVuby5lcnJvcnMuVW5leHBlY3RlZEVvZigpO1xuICByZXR1cm4gKGhpZ2ggPDwgMTYpIHwgbG93O1xufVxuIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBLDBFQUEwRTtBQUcxRSxTQUFTLFNBQVMsUUFBUSxrQkFBa0I7QUFFNUM7Ozs7O0NBS0MsR0FDRCxPQUFPLGVBQWUsUUFBUSxHQUFjO0VBQzFDLE1BQU0sT0FBTyxNQUFNLFVBQVU7RUFDN0IsSUFBSSxTQUFTLE1BQU0sT0FBTztFQUMxQixNQUFNLE1BQU0sTUFBTSxVQUFVO0VBQzVCLElBQUksUUFBUSxNQUFNLE1BQU0sSUFBSSxLQUFLLE1BQU0sQ0FBQyxhQUFhO0VBQ3JELE9BQU8sQUFBQyxRQUFRLEtBQU07QUFDeEIifQ==
// denoCacheMetadata=2290216488764502330,7912894046919081501