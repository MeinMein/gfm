module gfm.opengl.textureunit;

import derelict.opengl3.gl3;

import gfm.core.log,  
       gfm.opengl.opengl;

/// Cache state of OpenGL texture units.
/// Use deprecated image units!
final class TextureUnits
{
    public
    {
        /// Creates a TextureUnits object.
        /// This is automatically done by loading OpenGL.
        this(OpenGL gl)
        {
            _gl = gl;
            _activeTexture = -1; // default is unknown

            // Use the max total image units
            // Note: each shader stage has its own max to deal with
            int units = gl.maxCombinedImageUnits();

            _textureUnits.length = units;
            for (int i = 0; i < units; ++i)
                _textureUnits[i] = new TextureUnit(gl, i);
        }

        /// Sets the "active texture" which is more precisely active texture unit.
        /// Throws: $(D OpenGLException) on error.
        void setActiveTexture(int texture)
        {
            if (_textureUnits.length == 1)
                return;

            if (glActiveTexture is null)
                return;

            // cached value can be wrong when glActiveTexture is called outside gfm
            //if (_activeTexture != texture)
            //{
                glActiveTexture(GL_TEXTURE0 + texture);
                _gl.runtimeCheck();
                _activeTexture = texture;
            //}
        }

        /// Gets texture unit i.
        TextureUnit unit(int i)
        {
            return _textureUnits[i];
        }

        /// Gets the active texture unit.
        TextureUnit current()
        {
            if (_activeTexture == -1)
                setActiveTexture(0);

            return _textureUnits[_activeTexture];
        }
    }

    private
    {
        OpenGL _gl;
        int _activeTexture;          // index of currently active texture unit
        TextureUnit[] _textureUnits; // all texture units
    }
}

/// Cache state of a single OpenGL texture unit.
final class TextureUnit
{
    public
    {
        /// Binds this texture unit lazily.
        /// Throws: $(D OpenGLException) on error.
        void bind(GLenum target, GLuint texture)
        {
            size_t index = targetToIndex(cast(Target)target);
            // Fixed: Just because it's the same target and handle, does not mean
            // it's the same texture. So always bind.
            //if(_currentBinding[index] != texture)
            //{
                glBindTexture(target, texture);
                _gl.runtimeCheck();
                _currentBinding[index] = texture;
            //}
        }
    }

    private
    {
        this(OpenGL gl, int index)
        {
            _gl = gl;
            _index = index;

            _currentBinding[] = -1; // default is unknown
        }

        enum Target : GLenum
        {
            TEXTURE_1D = GL_TEXTURE_1D, 
            TEXTURE_2D = GL_TEXTURE_2D, 
            TEXTURE_3D = GL_TEXTURE_3D, 
            TEXTURE_1D_ARRAY = GL_TEXTURE_1D_ARRAY, 
            TEXTURE_2D_ARRAY = GL_TEXTURE_2D_ARRAY, 
            TEXTURE_RECTANGLE = GL_TEXTURE_RECTANGLE, 
            TEXTURE_BUFFER = GL_TEXTURE_BUFFER, 
            TEXTURE_CUBE_MAP = GL_TEXTURE_CUBE_MAP,
            TEXTURE_CUBE_MAP_ARRAY = GL_TEXTURE_CUBE_MAP_ARRAY, 
            TEXTURE_2D_MULTISAMPLE = GL_TEXTURE_2D_MULTISAMPLE, 
            TEXTURE_2D_MULTISAMPLE_ARRAY = GL_TEXTURE_2D_MULTISAMPLE_ARRAY 
        }

        size_t targetToIndex(Target target)
        {
            final switch(target)
            {
                case Target.TEXTURE_1D: return 0;
                case Target.TEXTURE_2D: return 1;
                case Target.TEXTURE_3D: return 2;
                case Target.TEXTURE_1D_ARRAY: return 3;
                case Target.TEXTURE_2D_ARRAY: return 4;
                case Target.TEXTURE_RECTANGLE: return 5;
                case Target.TEXTURE_BUFFER: return 6;
                case Target.TEXTURE_CUBE_MAP: return 7;
                case Target.TEXTURE_CUBE_MAP_ARRAY: return 8;
                case Target.TEXTURE_2D_MULTISAMPLE: return 9;
                case Target.TEXTURE_2D_MULTISAMPLE_ARRAY: return 10;
            }
        }

        OpenGL _gl;
        int _index;

        GLuint[Target.max + 1] _currentBinding;
    }
}
