%%
%%  Copyright (c) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy
%%  of this software and associated documentation files (the "Software"), to
%%  deal in the Software without restriction, including without limitation the
%%  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%  sell copies of the Software, and to permit persons to whom the Software is
%%  furnished to do so, subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in
%%  all copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%  IN THE SOFTWARE.
%%

-module(multipart_formdata).

-export([request/4]).

-define(Bound, "----------------ce9e5a76d5fc4f83").

request(Text, Bin, Type, Token) when is_binary(Bin) ->
    Url = "https://api.douban.com/shuo/v2/statuses/",
    ContentType = "multipart/form-data; boundary=" ++ 
                   lists:sublist(?Bound, 3, length(?Bound)),
    Part1 = lists:concat([?Bound,
            "\r\nContent-Disposition: form-data; name=\"text\"; \r\n\r\n", 
            Text, "\r\n"]),
    Part2 = lists:concat([?Bound,
            "\r\nContent-Disposition: form-data; name=\"image\"; filename=\"_\"",
            "\r\nContent-Type: image/", Type, "\r\n\r\n", binary_to_list(Bin)]),
    Body = lists:append(Part1, Part2),
    Len = integer_to_list(length(Body)),
    Headers = [{"Authorization", "Bearer "++Token}, {"Content-Length", Len}],
    httpc:request(post, {Url, Headers, ContentType, Body},
                  [], [{full_result, false}]).
