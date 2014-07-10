module gfm.sdl2.renderer;

import std.string;

import derelict.sdl2.sdl;

import gfm.core.log,
       gfm.core.text,
       gfm.math.vector,
       gfm.math.box,
       gfm.sdl2.sdl,
       gfm.sdl2.window,
       gfm.sdl2.texture,
       gfm.sdl2.surface;

/// SDL Renderer wrapper.
final class SDL2Renderer
{
    public
    {
        /// Creates a SDL renderer which targets a window.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_CreateRenderer)
        /// Throws: $(D SDL2Exception) on error.
        this(SDL2Window window, int flags)
        {
            _sdl2 = window._sdl2;
            _renderer = SDL_CreateRenderer(window._window, -1, flags);
            if (_renderer is null)
                _sdl2.throwSDL2Exception("SDL_CreateRenderer");
        }

        /// Create a software renderer which targets a surface.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_CreateSoftwareRenderer)
        /// Throws: $(D SDL2Exception) on error.
        this(SDL2Surface surface)
        {
            _sdl2 = surface._sdl2;
            _renderer = SDL_CreateSoftwareRenderer(surface._surface);
            if (_renderer is null)
                _sdl2.throwSDL2Exception("SDL_CreateSoftwareRenderer");
        }

        /// Releases the SDL ressource.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_DestroyRenderer)
        void close()
        {
            if (_renderer !is null)
            {
                SDL_DestroyRenderer(_renderer);
                _renderer = null;
            }
        }

        ~this()
        {
            close();
        }

        /// Clear the current rendering target with the drawing color.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderClear)
        /// Throws: $(D SDL2Exception) on error.
        void clear()
        {
            if (0 != SDL_RenderClear(_renderer))
                _sdl2.throwSDL2Exception("SDL_RenderClear");
        }

        /// Update the screen with rendering performed.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderPresent)
        void present()
        {
            SDL_RenderPresent(_renderer);
        }

        /// Sets the color used for drawing operations.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_SetRenderDrawColor)
        /// Throws: $(D SDL2Exception) on error.
        void setColor(ubyte r, ubyte g, ubyte b, ubyte a)
        {
            if (0 != SDL_SetRenderDrawColor(_renderer, r, g, b, a))
                _sdl2.throwSDL2Exception("SDL_SetRenderDrawColor");
        }

        /// Sets the window drawing area.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderSetViewport)
        /// Throws: $(D SDL2Exception) on error.
        void setViewport(box2i b)
        {
            SDL_Rect r = box2i_to_SDL_Rect(b);
            if (0 != SDL_RenderSetViewport(_renderer, &r))
                _sdl2.throwSDL2Exception("SDL_RenderSetViewport");
        }

        /// Sets the whole window as drawing area.        
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderSetViewport)
        /// Throws: $(D SDL2Exception) on error.
        void setViewportFull()
        {
            if (0 != SDL_RenderSetViewport(_renderer, null))
                _sdl2.throwSDL2Exception("SDL_RenderSetViewport");
        }

        /// Sets SDL blend mode.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_SetRenderDrawBlendMode)
        /// Throws: $(D SDL2Exception) on error.
        void setBlend(int blendMode)
        {
            if (0 != SDL_SetRenderDrawBlendMode(_renderer, blendMode))
                _sdl2.throwSDL2Exception("SDL_SetRenderDrawBlendMode");
        }

        /// Draw a line.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderDrawLine)
        /// Throws: $(D SDL2Exception) on error.
        void drawLine(vec2i a, vec2i b)
        {
            if (0 != SDL_RenderDrawLine(_renderer, a.x, a.y, b.x, b.y))
                _sdl2.throwSDL2Exception("SDL_RenderDrawLine");

        }

        /// Draw several lines at once.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderDrawLines)
        /// Throws: $(D SDL2Exception) on error.
        void drawLines(vec2i[] points)
        {
            if (0 != SDL_RenderDrawLines(_renderer, cast(SDL_Point*)(points.ptr), cast(int)(points.length)))
                _sdl2.throwSDL2Exception("SDL_RenderDrawLines");
        }

        /// Draw a point.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderDrawPoint)
        /// Throws: $(D SDL2Exception) on error.
        void drawPoint(vec2i point)
        {
            if (0 != SDL_RenderDrawPoint(_renderer, point.x, point.y))
                _sdl2.throwSDL2Exception("SDL_RenderDrawPoint");
        }

        /// Draw several point at once.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderDrawPoints)
        /// Throws: $(D SDL2Exception) on error.
        void drawPoints(vec2i[] points)
        {
            if (0 != SDL_RenderDrawPoints(_renderer, cast(SDL_Point*)(points.ptr), cast(int)(points.length)))
                _sdl2.throwSDL2Exception("SDL_RenderDrawPoints");
        }

        /// Draw a rectangle outline.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderDrawRect)
        /// Throws: $(D SDL2Exception) on error.
        void drawRect(box2i rect)
        {
            SDL_Rect r = box2i_to_SDL_Rect(rect);
            if (0 != SDL_RenderDrawRect(_renderer, &r))
                _sdl2.throwSDL2Exception("SDL_RenderDrawRect");
        }

        /// Draw a filled rectangle.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderFillRect)
        /// Throws: $(D SDL2Exception) on error.
        void fillRect(box2i rect)
        {
            SDL_Rect r = box2i_to_SDL_Rect(rect);
            if (0 != SDL_RenderFillRect(_renderer, &r))
                _sdl2.throwSDL2Exception("SDL_RenderFillRect");
        }

        /// Blit a rectangle from a texture.
        /// See_also: $(LINK http://wiki.libsdl.org/SDL_RenderCopy)
        /// Throws: $(D SDL2Exception) on error.
        void copy(SDL2Texture texture, box2i srcRect, box2i dstRect)
        {
            auto f = texture.format();
            SDL_Rect src = box2i_to_SDL_Rect(srcRect);
            SDL_Rect dst = box2i_to_SDL_Rect(dstRect);
            if (0 != SDL_RenderCopy(_renderer, texture._handle, &src, &dst))
                _sdl2.throwSDL2Exception("SDL_RenderCopy");
        }

		SDL2RendererInfo getRendererInfo()
		{
			SDL_RendererInfo info;
			if (0 != SDL_GetRendererInfo(_renderer, &info))
				_sdl2.throwSDL2Exception("SDL_GetRendererInfo");
			return new SDL2RendererInfo(_sdl2._log, 0, info);
		}
    }

    package
    {
        SDL2 _sdl2;
        SDL_Renderer* _renderer;
    }

    private
    {
        static SDL_Rect box2i_to_SDL_Rect(box2i b) pure
        {
            SDL_Rect res = void;
            res.x = b.min.x;
            res.y = b.min.y;
            res.w = b.width;
            res.h = b.height;
            return res;
        }
    }
}

/// SDL Renderer information.
final class SDL2RendererInfo
{
    public
    {
        this(Log log, int index, SDL_RendererInfo info)
        {
            _log = log;
            _index = index;
            _info = info;
        }

        /// Returns: Renderer name.
        string name()
        {
            return sanitizeUTF8(_info.name, _log, "SDL2 renderer name");
        }

        /// Returns: true if this renderer is software.
        bool isSoftware()
        {
            return (_info.flags & SDL_RENDERER_SOFTWARE) != 0;
        }

        /// Returns: true if this renderer is accelerated.
        bool isAccelerated()
        {
            return (_info.flags & SDL_RENDERER_ACCELERATED) != 0;
        }

        /// Returns: true if this renderer can render to a texture.
        bool hasRenderToTexture()
        {
            return (_info.flags & SDL_RENDERER_TARGETTEXTURE) != 0;
        }

        /// Returns: true if this renderer support vertical synchronization.
        bool isVsyncEnabled()
        {
            return (_info.flags & SDL_RENDERER_PRESENTVSYNC) != 0;
        }

        /// Returns: Pretty string describing the renderer.
        override string toString()
        {
            string res = format("renderer #%d: %s [flags:", _index, name());
            if (isSoftware()) res ~= " software";
            if (isAccelerated()) res ~= " accelerated";
            if (hasRenderToTexture()) res ~= " render-to-texture";
            if (isVsyncEnabled()) res ~= " vsync";
            res ~= "]\n";
            res ~= format("max. texture: %sx%s", _info.max_texture_width, _info.max_texture_height);
            return res;
        }
    }

    private
    {
        Log _log;
        int _index;
        SDL_RendererInfo _info;
    }
}
