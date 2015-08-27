module gfm.sdl2.window;

import std.string;

import derelict.sdl2.sdl;

import gfm.core.log,
       gfm.math.vector,
       gfm.math.box,
       gfm.sdl2.sdl,
       gfm.sdl2.surface;

/// SDL Window wrapper.
/// There is two ways to receive events, either by polling a SDL2 object, 
/// or by overriding the event callbacks.
class SDL2Window
{
    public
    {
        /// Initially invisible.
        /// Accepts the same constants as the SDL2 function.
        this(SDL2 sdl2, int x, int y, int width, int height, int flags)
        {
            _sdl2 = sdl2;
            _log = sdl2._log;
            _surface = null;
            _glContext = null;
            _surfaceMustBeRenewed = false;

            bool OpenGL = (flags & SDL_WINDOW_OPENGL) != 0;

            if (OpenGL)
            {
                // force debug OpenGL context creation in debug mode
                debug
                {
                    SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG);
                }

                // put here your desired context profile and version

                SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
                SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
                //SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG | SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
            }

            _window = SDL_CreateWindow(toStringz(""), x, y, width, height, flags);
            if (_window == null)
                throw new SDL2Exception("SDL_CreateWindow failed: " ~ _sdl2.getErrorString());

            _id = SDL_GetWindowID(_window);

            // register window for event dispatch
            _sdl2.registerWindow(this);

            if (OpenGL)
                _glContext = new SDL2GLContext(this);
        }

        /// Releases the SDL resource.
        final void close()
        {
            if (_glContext !is null)
            {
                _glContext.close();
                _glContext = null;
            }

            if (_window !is null)
            {
                _sdl2.unregisterWindow(this);
                SDL_DestroyWindow(_window);
                _window = null;
            }
        }

        ~this()
        {
            close();
        }

        final void setFullscreen(bool activated)
        {
            SDL_SetWindowFullscreen(_window, activated ? SDL_WINDOW_FULLSCREEN : 0);
        }

        final void setPosition(vec2i position)
        {
            SDL_SetWindowPosition(_window, position.x, position.y);
        }

        final void setSize(vec2i size)
        {
            SDL_SetWindowSize(_window, size.x, size.y);
        }

        final vec2i getSize()
        {
            int w, h;
            SDL_GetWindowSize(_window, &w, &h);
            return vec2i(w, h);
        }

        final void setTitle(string title)
        {
            SDL_SetWindowTitle(_window, toStringz(title));
        }

        final void show()
        {
            SDL_ShowWindow(_window);
        }

        final void hide()
        {
            SDL_HideWindow(_window);
        }

        final void minimize()
        {
            SDL_MinimizeWindow(_window);
        }

        final void maximize()
        {
            SDL_MaximizeWindow(_window);
        }

        final SDL2Surface surface()
        {
            if (!hasValidSurface())
            {
                SDL_Surface* internalSurface = SDL_GetWindowSurface(_window);
                if (internalSurface is null)
                    _sdl2.throwSDL2Exception("SDL_GetWindowSurface");

                // renews surface as needed
                _surfaceMustBeRenewed = false;
                _surface = new SDL2Surface(_sdl2, internalSurface,  SDL2Surface.Owned.NO);
            }
            return _surface;
        }

        final void updateSurface()
        {
            if (!hasValidSurface())
                surface();

            int res = SDL_UpdateWindowSurface(_window);
            if (res != 0)
                _sdl2.throwSDL2Exception("SDL_UpdateWindowSurface");
            
        }

        final int id()
        {
            return _id;
        }


        // override these function, they are event callbacks

        void onShow()
        {
        }

        void onHide()
        {
        }

        void onExposed()
        {
            _surfaceMustBeRenewed = true;
        }

        void onMove(int x, int y)
        {        
        }
        
        void onResized(int width, int height)
        {
            _surfaceMustBeRenewed = true;
        }

        void onSizeChanged()
        {
            _surfaceMustBeRenewed = true;
        }

        void onMinimized()
        {
            _surfaceMustBeRenewed = true;
        }

        void onMaximized()
        {
            _surfaceMustBeRenewed = true;
        }

        void onRestored()
        {            
        }

        void onEnter()
        {
        }
        
        void onLeave()
        {
        }
        
        void onFocusGained()
        {
        }

        void onFocusLost()
        {
        }
        
        void onClose()
        {
        }

        void swapBuffers()
        {
            if (_glContext is null)
                throw new SDL2Exception("swapBuffers failed: not an OpenGL window");
            SDL_GL_SwapWindow(_window);
        }
    }

    package
    {
        SDL2 _sdl2;
        SDL_Window* _window;
    }

    private
    {
        Log _log;
        SDL2Surface _surface;
        SDL2GLContext _glContext;
        uint _id;

        bool _surfaceMustBeRenewed;

        bool hasValidSurface()
        {
            return (!_surfaceMustBeRenewed) && (_surface !is null);
        }
    }
}

/// SDL OpenGL context wrapper.
final class SDL2GLContext
{
    public
    {
        this(SDL2Window window)
        {
            _window = window;
            _context = SDL_GL_CreateContext(window._window);
            _initialized = true;
        }

        ~this()
        {
            close();
        }

        void close()
        {
            if (_initialized)
            {
                // work-around Issue #19
                // SDL complains with log message "wglMakeCurrent(): The handle is invalid."
                // in the SDL_DestroyWindow() call if we destroy the OpenGL context before-hand
                //
                // SDL_GL_DeleteContext(_context);
                _initialized = false;
            }
        }

        void makeCurrent()
        {
            if (0 != SDL_GL_MakeCurrent(_window._window, _context))
                _window._sdl2.throwSDL2Exception("SDL_GL_MakeCurrent");
        }
    }

    package
    {
        SDL_GLContext _context;
        SDL2Window _window;
    }

    private
    {
        bool _initialized;
    }
}

