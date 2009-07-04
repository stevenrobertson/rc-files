-- Standard awesome library
require("awful")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
-- The default is a dark theme
--theme_path = "/home/steven/.config/awesome/themes/aurora-looks-theme"
theme_path = "/usr/share/awesome/themes/default/theme.lua"
-- Uncommment this for a lighter theme
-- theme_path = "/usr/share/awesome/themes/sky/theme"

-- Actually load theme
beautiful.init(theme_path)

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
k_a = {"Mod1"}
k_c = {"Control"}
k_ac = {"Mod1", "Control"}
k_w = {"Mod4"}
k_wc = {"Mod4", "Control"}

-- This is used later as the default terminal to run.
-- terminal = "xterm -j -s -si -sk -sl 1000 -vb -bg '#000' -fg '#f6f3e8' -fa 'Droid Sans Mono' -fs 8 -geometry 104x28 -tn xterm-color -ls"
terminal = 'xterm'
editor_cmd = "gvim"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating
}

-- Table of clients that should be set floating. The index may be either
-- the application class or instance. The instance is useful when running
-- a console app in a terminal like (Music on Console)
--    xterm -name mocp -e mocp
floatapps =
{
    -- by class
    ["MPlayer"] = true,
    ["pinentry"] = true,
    ["gimp"] = true,
    ["TopCoder"] = true,
    -- by instance
    ["Xdialog"] = true,
    ["mocp"] = true,
    ["com.topcoder.client.contestApplet.runner.generic"] = true,
    ["firefox"] = false
}

-- Applications to be moved to a pre-defined tag by class or instance.
-- Use the screen and tags indices.
apptags =
{
    -- ["Quod Libet"] = { screen = 2, tag = 9 }
    -- ["Firefox"] = { screen = 1, tag = 2 },
    -- ["mocp"] = { screen = 2, tag = 4 },
}

-- Define if we want to use titlebar on all applications.
use_titlebar = false
-- }}}

-- {{{ Tags
-- Define tags table.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = {}
    -- Create 9 tags per screen.
    for tagnumber = 1, 9 do
        tags[s][tagnumber] = tag(tagnumber)
        -- Add tags to screen one by one
        tags[s][tagnumber].screen = s
        awful.layout.set(layouts[5], tags[s][tagnumber])
    end
    -- I'm sure you want to see at least one tag.
    tags[s][1].selected = true
end
-- }}}

-- {{{ Wibox
-- Create a textbox widget
mytextbox = widget({ type = "textbox", align = "right" })
-- Set the default text in textbox
mytextbox.text = "<b><small> " .. awesome.release .. " </small></b>"

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu.new({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                        { "open terminal", terminal }
                                      }
                            })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })

-- Create a systray
mysystray = widget({ type = "systray", align = "right" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
			awful.button({ }, 1, awful.tag.viewonly),
			awful.button({ modkey }, 1, awful.client.movetotag),
			awful.button({ }, 3, function (tag) tag.selected = not tag.selected end),
			awful.button({ modkey }, 3, awful.client.toggletag),
			awful.button({ }, 5, awful.tag.viewnext),
			awful.button({ }, 4, awful.tag.viewprev)
                    )

mytasklist = {}

mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))
for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ align = "left" })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = widget({ type = "imagebox", align = "right" })
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)


    -- Create the wibox
    mywibox[s] = wibox({ position = "top", fg = beautiful.fg_normal, bg = beautiful.bg_normal })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = { mylauncher,
                           mytaglist[s],
                           mytasklist[s],
                           mypromptbox[s],
                           mytextbox,
                           mylayoutbox[s],
                           s == screen.count() and mysystray or nil }
    mywibox[s].screen = s
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 5, awful.tag.viewnext),
    awful.button({ }, 4, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings

globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
    awful.key({ "Mod1", "Control" }, "Left",   awful.tag.viewprev       ),
    awful.key({ "Mod1", "Control" }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ "Mod1", "Control" }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ "Mod1", "Control" }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation
    awful.key({ modkey, "Control" }, "j", function () awful.client.swap.byidx(  1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.client.swap.byidx( -1) end),
    awful.key({ modkey, "Mod1"    }, "j", function () awful.screen.focus( 1)       end),
    awful.key({ modkey, "Mod1"    }, "k", function () awful.screen.focus(-1)       end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    
    --[[key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),
    ]]--

    -- Standard program
    awful.key({ "Mod1", "Control" }, "t", function () awful.util.spawn(terminal) end),
    awful.key({ "Mod1", "Control" }, "i", function () awful.util.spawn('firefox') end),
    awful.key({ "Mod1", "Control" }, "e", function () awful.util.spawn('gvim') end),
    awful.key({ "Mod1", "Control" }, "q", function () awful.util.spawn('quodlibet') end),
    awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn("am '1.5dB-'") end),
    awful.key({}, "XF86AudioRaiseVolume", function () awful.util.spawn("am '1.5dB+'") end),
    awful.key({}, "XF86AudioMute",        function () awful.util.spawn("amixer sset Master 0") end),

    awful.key({ modkey, "Mod1"      }, "r", awesome.restart),
    --awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ "Mod1", "Control" }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ "Mod1", "Control" }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Mod1"    }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Mod1"    }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ "Mod1", "Control" }, "space", function () awful.layout.inc(layouts,  1) end),

    -- Prompt
    awful.key({ "Mod1", "Control" }, "r",     function () mypromptbox[mouse.screen]:run() end),
    awful.key({ modkey, "Control" }, "r",
        function ()
            awful.prompt.run({ prompt = "Run Lua code: " },
            mypromptbox[mouse.screen].widget,
            awful.util.eval, nil,
            awful.util.getdir("cache") .. "/history_eval")
        end)
)

-- Client awful tagging: this is useful to tag some clients and then do stuff like move to tag on them
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ "Mod1", "Control" }, "w",      function (c) c:kill()                         end),
    awful.key({ "Mod1", "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey }, "t", awful.client.togglemarked),
    awful.key({ modkey,}, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ }, "F" .. i,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ "Shift" }, "F" .. i,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          tags[screen][i].selected = not tags[screen][i].selected
                      end
                  end),
        awful.key({ "Mod1" }, "F" .. i,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ "Mod1", "Shift" }, i,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end),
	awful.key({ modkey, "Control" }, "i", function ()
                                        local s = mouse.screen
                                        if mypromptbox[s].text then
                                            mypromptbox[s].text = nil
                                        elseif client.focus then
                                            mypromptbox[s].text = nil
                                            if client.focus.class then
                                                mypromptbox[s].text = "Class: " .. client.focus.class .. " "
                                            end
                                            if client.focus.instance then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Instance: ".. client.focus.instance .. " "
                                            end
                                            if client.focus.role then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Role: ".. client.focus.role
                                            end
                                        end
                                    end))
end

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Hooks
-- Hook function to execute when focusing a client.
awful.hooks.focus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_focus
    end
end)

-- Hook function to execute when unfocusing a client.
awful.hooks.unfocus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_normal
    end
end)

-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
    c.border_color = beautiful.border_marked
end)

-- Hook function to execute when unmarking a client.
awful.hooks.unmarked.register(function (c)
    c.border_color = beautiful.border_focus
end)

-- Hook function to execute when the mouse enters a client.
awful.hooks.mouse_enter.register(function (c)
    -- Sloppy focus, but disabled for magnifier layout
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- Hook function to execute when a new client appears.
awful.hooks.manage.register(function (c, startup)
    -- If we are not managing this application at startup,
    -- move it to the screen where the mouse is.
    -- We only do it for filtered windows (i.e. no dock, etc).
    if not startup and awful.client.focus.filter(c) then
        c.screen = mouse.screen
    end

    if use_titlebar then
        -- Add a titlebar
        awful.titlebar.add(c, { modkey = modkey })
    end
    -- Add mouse bindings
    c:buttons(awful.util.table.join(
        awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
        awful.button({ modkey }, 1, awful.mouse.client.move),
        awful.button({ modkey }, 3, awful.mouse.client.resize)
    ))
    -- New client may not receive focus
    -- if they're not focusable, so set border anyway.
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_normal

    -- Check if the application should be floating.
    local cls = c.class
    local inst = c.instance
    if floatapps[cls] ~= nil then
        awful.client.floating.set(c, floatapps[cls])
    elseif floatapps[inst] ~= nil then
        awful.client.floating.set(c, floatapps[inst])
    end

    -- Check application->screen/tag mappings.
    local target
    if apptags[cls] then
        target = apptags[cls]
    elseif apptags[inst] then
        target = apptags[inst]
    end
    if target then
        c.screen = target.screen
        awful.client.movetotag(tags[target.screen][target.tag], c)
    end

    -- Do this after tag mapping, so you don't see it on the wrong tag for a split second.
    client.focus = c

    -- Set key bindings
    c:keys(clientkeys)

    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Honor size hints: if you want to drop the gaps between windows, set this to false.
    -- c.size_hints_honor = false
end)

-- Hook function to execute when arranging the screen.
-- (tag switch, new client, etc)
awful.hooks.arrange.register(function (screen)
    local layout = awful.layout.getname(awful.layout.get(screen))
    if layout and beautiful["layout_" ..layout] then
        mylayoutbox[screen].image = image(beautiful["layout_" .. layout])
    else
        mylayoutbox[screen].image = nil
    end

    -- Give focus to the latest client in history if no window has focus
    -- or if the current window is a desktop or a dock one.
    if not client.focus then
        local c = awful.client.focus.history.get(screen, 0)
        if c then client.focus = c end
    end
end)

-- Hook called every second
awful.hooks.timer.register(1, function ()
    -- For unix time_t lovers
    -- mytextbox.text = " " .. os.time() .. " time_t "
    -- Otherwise use:
    mytextbox.text = " "  .. os.date() .. " "
end)

mybottombar = wibox({ position = "bottom", fg=beautiful.fg_normal, bg = beautiful.bg_normal})
mybottombar.screen = 2 -- screen.count()

myquodbox = widget({ type = "textbox", align = "left" })
myquodbox.text = ""

awful.hooks.timer.register(10, function ()
   local current = {}
   local currentfile = io.open("/home/steven/.quodlibet/current")
   if not currentfile then return end
   for line in currentfile:lines() do
      local k, v
      k, v = line:match('\([^=]*\)=\(.*\)')
      current[k] = v:gsub('&', '&amp;')
   end
   currentfile:close()
   if current.title ~= nil then
      myquodbox.text = " " .. current.artist .. " - <b>" .. current.title .. "</b> <i>track " .. current.tracknumber .. " on " .. current.album .. "</i> "
   else
      myquodbox.text = ""
   end

end)

myloadbox = widget({ type = "textbox", align = "right" })
myloadbox.text = ""

awful.hooks.timer.register(3, function ()
   local loadavg = io.open("/proc/loadavg")
   if loadavg then
      myloadbox.text = " " .. loadavg:read("*l") .. " "
      loadavg:close()
   end
end)

mygmailbox = widget({ type = "textbox", align = "right" })
mygmailbox.text = ""

--awful.hooks.timer.register(50, function ()
--   local gmail = io.open("/home/steven/.mail.txt")
--   mygmailbox.text = gmail:read("*l")
--   gmail:close()
--end)

mybottombar.widgets = {myquodbox, mygmailbox, myloadbox}

--awful.hooks.timer.register(3600 * 4, awesome.restart)
