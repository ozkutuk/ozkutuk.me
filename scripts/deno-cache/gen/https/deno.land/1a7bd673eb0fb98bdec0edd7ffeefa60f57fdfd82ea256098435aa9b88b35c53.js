// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
/**
 * A `LimitedReader` reads from `reader` but limits the amount of data returned to just `limit` bytes.
 * Each call to `read` updates `limit` to reflect the new amount remaining.
 * `read` returns `null` when `limit` <= `0` or
 * when the underlying `reader` returns `null`.
 */ /**
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export class LimitedReader {
  reader;
  limit;
  constructor(reader, limit){
    this.reader = reader;
    this.limit = limit;
  }
  async read(p) {
    if (this.limit <= 0) {
      return null;
    }
    if (p.length > this.limit) {
      p = p.subarray(0, this.limit);
    }
    const n = await this.reader.read(p);
    if (n === null) {
      return null;
    }
    this.limit -= n;
    return n;
  }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL2xpbWl0ZWRfcmVhZGVyLnRzIl0sInNvdXJjZXNDb250ZW50IjpbIi8vIENvcHlyaWdodCAyMDE4LTIwMjQgdGhlIERlbm8gYXV0aG9ycy4gQWxsIHJpZ2h0cyByZXNlcnZlZC4gTUlUIGxpY2Vuc2UuXG4vLyBUaGlzIG1vZHVsZSBpcyBicm93c2VyIGNvbXBhdGlibGUuXG5cbi8qKlxuICogQSBgTGltaXRlZFJlYWRlcmAgcmVhZHMgZnJvbSBgcmVhZGVyYCBidXQgbGltaXRzIHRoZSBhbW91bnQgb2YgZGF0YSByZXR1cm5lZCB0byBqdXN0IGBsaW1pdGAgYnl0ZXMuXG4gKiBFYWNoIGNhbGwgdG8gYHJlYWRgIHVwZGF0ZXMgYGxpbWl0YCB0byByZWZsZWN0IHRoZSBuZXcgYW1vdW50IHJlbWFpbmluZy5cbiAqIGByZWFkYCByZXR1cm5zIGBudWxsYCB3aGVuIGBsaW1pdGAgPD0gYDBgIG9yXG4gKiB3aGVuIHRoZSB1bmRlcmx5aW5nIGByZWFkZXJgIHJldHVybnMgYG51bGxgLlxuICovXG5pbXBvcnQgdHlwZSB7IFJlYWRlciB9IGZyb20gXCIuL3R5cGVzLnRzXCI7XG5cbi8qKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGNsYXNzIExpbWl0ZWRSZWFkZXIgaW1wbGVtZW50cyBSZWFkZXIge1xuICBjb25zdHJ1Y3RvcihwdWJsaWMgcmVhZGVyOiBSZWFkZXIsIHB1YmxpYyBsaW1pdDogbnVtYmVyKSB7fVxuXG4gIGFzeW5jIHJlYWQocDogVWludDhBcnJheSk6IFByb21pc2U8bnVtYmVyIHwgbnVsbD4ge1xuICAgIGlmICh0aGlzLmxpbWl0IDw9IDApIHtcbiAgICAgIHJldHVybiBudWxsO1xuICAgIH1cblxuICAgIGlmIChwLmxlbmd0aCA+IHRoaXMubGltaXQpIHtcbiAgICAgIHAgPSBwLnN1YmFycmF5KDAsIHRoaXMubGltaXQpO1xuICAgIH1cbiAgICBjb25zdCBuID0gYXdhaXQgdGhpcy5yZWFkZXIucmVhZChwKTtcbiAgICBpZiAobiA9PT0gbnVsbCkge1xuICAgICAgcmV0dXJuIG51bGw7XG4gICAgfVxuXG4gICAgdGhpcy5saW1pdCAtPSBuO1xuICAgIHJldHVybiBuO1xuICB9XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQzs7Ozs7Q0FLQyxHQUdEOztDQUVDLEdBQ0QsT0FBTyxNQUFNOzs7RUFDWCxZQUFZLEFBQU8sTUFBYyxFQUFFLEFBQU8sS0FBYSxDQUFFO1NBQXRDLFNBQUE7U0FBdUIsUUFBQTtFQUFnQjtFQUUxRCxNQUFNLEtBQUssQ0FBYSxFQUEwQjtJQUNoRCxJQUFJLElBQUksQ0FBQyxLQUFLLElBQUksR0FBRztNQUNuQixPQUFPO0lBQ1Q7SUFFQSxJQUFJLEVBQUUsTUFBTSxHQUFHLElBQUksQ0FBQyxLQUFLLEVBQUU7TUFDekIsSUFBSSxFQUFFLFFBQVEsQ0FBQyxHQUFHLElBQUksQ0FBQyxLQUFLO0lBQzlCO0lBQ0EsTUFBTSxJQUFJLE1BQU0sSUFBSSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUM7SUFDakMsSUFBSSxNQUFNLE1BQU07TUFDZCxPQUFPO0lBQ1Q7SUFFQSxJQUFJLENBQUMsS0FBSyxJQUFJO0lBQ2QsT0FBTztFQUNUO0FBQ0YifQ==
// denoCacheMetadata=5140473355729385783,7862162816714904063