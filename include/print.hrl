%%%-------------------------------------------------------------------
%%% @author Ahmad Baitalmal
%%% @copyright (C) 2017, Ahmad Baitalmal
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files
%%% (the “Software”), to deal in the Software without restriction,
%%% including without limitation the rights to use, copy, modify, merge,
%%% publish, distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%% @doc
%%% Pretty output printing.
%%% Usage:
%%%
%%% printing with ~s for all terms
%%% ?print("foreground      %k %r %g %y %b %m %c %w", [black, red, green, yellow, blue, magenta, cyan, white]),
%%% ?print("background      %K %R %G %Y %B %M %C %W", [black, red, green, yellow, blue, magenta, cyan, white]),
%%% ?print("effects         %e %d %i %u %l", [emphasis, dim, italic, underline, blink]),
%%% ?print("color effects   %er %dm %ig %uy %lc", [emphasis_red, dim_magenta, italic_green, underline_yellow, blink_cyan]),
%%%
%%% printing with ~ts for strings
%%% ?print("foreground      $k $r $g $y $b $m $c $w", [<<"black">>, <<"red">>, <<"green">>, <<"yellow">>, <<"blue">>, <<"magenta">>, <<"cyan">>, <<"white">>]),
%%% ?print("background      $K $R $G $Y $B $M $C $W", [<<"black">>, <<"red">>, <<"green">>, <<"yellow">>, <<"blue">>, <<"magenta">>, <<"cyan">>, <<"white">>]),
%%% ?print("effects         $e $d $i $u $l", [<<"emphasis">>, <<"dim">>, <<"italic">>, <<"underline">>, <<"blink">>]),
%%% ?print("color effects   $er $dm $ig $uy $lc", [<<"emphasis_red">>, <<"dim_magenta">>, <<"italic_green">>, <<"underline_yellow">>, <<"blink_cyan">>]),
%%%
%%% A good reference
%%% http://misc.flogisoft.com/bash/tip_colors_and_formatting
%%% @end
%%% Created : 20. Jan 2017 6:43 AM
%%%-------------------------------------------------------------------


-define(_print_colors, [
    % effects with colors
    {"\\%e([krgybmcwnKRGYBMCWN])", "\e[1m\\%\\1\e[00m"},   % emphasis bold
    {"\\%d([krgybmcwnKRGYBMCWN])", "\e[2m\\%\\1\e[22m"},   % dim
    {"\\%i([krgybmcwnKRGYBMCWN])", "\e[3m\\%\\1\e[23m"},   % italic
    {"\\%u([krgybmcwnKRGYBMCWN])", "\e[4m\\%\\1\e[24m"},   % underline
    {"\\%l([krgybmcwnKRGYBMCWN])", "\e[5m\\%\\1\e[25m"},   % blink
    % effects without colors
    {"\\%e", "\e[1m~s\e[00m"},   % emphasis bold
    {"\\%d", "\e[2m~s\e[22m"},   % dim
    {"\\%i", "\e[3m~s\e[23m"},   % italic
    {"\\%u", "\e[4m~s\e[24m"},   % underline
    {"\\%l", "\e[5m~s\e[25m"},   % blink
    % foreground
    {"\\%k", "\e[30m~s\e[39m"},   % black
    {"\\%r", "\e[31m~s\e[39m"},   % red
    {"\\%g", "\e[32m~s\e[39m"},   % green
    {"\\%y", "\e[33m~s\e[39m"},   % yellow
    {"\\%b", "\e[34m~s\e[39m"},   % blue
    {"\\%m", "\e[35m~s\e[39m"},   % magenta (purple)
    {"\\%c", "\e[36m~s\e[39m"},   % cyan
    {"\\%w", "\e[37m~s\e[39m"},   % cyan
    {"\\%n", "\e[39m~s\e[39m"},    % default
    % background
    {"\\%K", "\e[40m~s\e[49m"},   % black
    {"\\%R", "\e[30m\e[41m~s\e[49m\e[39m"},   % red
    {"\\%G", "\e[30m\e[42m~s\e[49m\e[39m"},   % green
    {"\\%Y", "\e[30m\e[43m~s\e[49m\e[39m"},   % yellow
    {"\\%B", "\e[30m\e[44m~s\e[49m\e[39m"},   % blue
    {"\\%M", "\e[30m\e[45m~s\e[49m\e[39m"},   % magenta (purple)
    {"\\%C", "\e[30m\e[46m~s\e[49m\e[39m"},   % cyan
    {"\\%W", "\e[30m\e[47m~s\e[49m\e[39m"},   % white
    {"\\%N", "\e[30m\e[49m~s\e[49m\e[39m"},    % default
%%
    % effects with colors
    {"\\$e([krgybmcwnKRGYBMCWN])", "\e[1m\\$\\1\e[00m"},   % emphasis bold
    {"\\$d([krgybmcwnKRGYBMCWN])", "\e[2m\\$\\1\e[22m"},   % dim
    {"\\$i([krgybmcwnKRGYBMCWN])", "\e[3m\\$\\1\e[23m"},   % italic
    {"\\$u([krgybmcwnKRGYBMCWN])", "\e[4m\\$\\1\e[24m"},   % underline
    {"\\$l([krgybmcwnKRGYBMCWN])", "\e[5m\\$\\1\e[25m"},   % blink
    % effects without colors
    {"\\$e", "\e[1m~ts\e[00m"},   % emphasis bold
    {"\\$d", "\e[2m~ts\e[22m"},   % dim
    {"\\$i", "\e[3m~ts\e[23m"},   % italic
    {"\\$u", "\e[4m~ts\e[24m"},   % underline
    {"\\$l", "\e[5m~ts\e[25m"},   % blink
    % foreground
    {"\\$k", "\e[30m~ts\e[39m"},   % black
    {"\\$r", "\e[31m~ts\e[39m"},   % red
    {"\\$g", "\e[32m~ts\e[39m"},   % green
    {"\\$y", "\e[33m~ts\e[39m"},   % yellow
    {"\\$b", "\e[34m~ts\e[39m"},   % blue
    {"\\$m", "\e[35m~ts\e[39m"},   % magenta (purple)
    {"\\$c", "\e[36m~ts\e[39m"},   % cyan
    {"\\$w", "\e[37m~ts\e[39m"},   % cyan
    {"\\$n", "\e[39m~ts\e[39m"},    % default
    % background
    {"\\$K", "\e[40m~ts\e[49m"},   % black
    {"\\$R", "\e[30m\e[41m~ts\e[49m\e[39m"},   % red
    {"\\$G", "\e[30m\e[42m~ts\e[49m\e[39m"},   % green
    {"\\$Y", "\e[30m\e[43m~ts\e[49m\e[39m"},   % yellow
    {"\\$B", "\e[30m\e[44m~ts\e[49m\e[39m"},   % blue
    {"\\$M", "\e[30m\e[45m~ts\e[49m\e[39m"},   % magenta (purple)
    {"\\$C", "\e[30m\e[46m~ts\e[49m\e[39m"},   % cyan
    {"\\$W", "\e[30m\e[47m~ts\e[49m\e[39m"},   % white
    {"\\$N", "\e[30m\e[49m~ts\e[49m\e[39m"}   % default
%%	,{"([^,])$","\\1~n"}  % add a ~n at the end by default unless we have a ,
]).
-define(print(Format),
    ?__do_print(Format, [])).
-define(print(Format, Args),
    ?__do_print(Format, Args)).
-define(__do_print(Format, Args),
    io:format(lists:foldl(fun({Pattern, Replacement}, Data) ->
        re:replace(Data, Pattern, Replacement, [{return, list}, global])
                          end,
        % if the formatting string starts with :, print the output on the same line as the function stamp
        case Format of
            [$, | _] -> tl(Format);
            [$: | _] -> "%d %d$d%dg$d%db %dc " ++ tl(Format);
            _ -> Format
        end,
        ?_print_colors),
        case Format of
            [$, | _] -> tl(tl(tl(tl(tl(tl(tl(Args))))))); _ -> Args end)).