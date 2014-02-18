module gfm.opengl.uniformblock;

import gfm.math.matrix;

import std.stdio;
import std.string;
import std.conv;
import std.traits, std.typetuple;

import std.c.string;

import derelict.opengl3.gl;

import gfm.math.vector;
import gfm.core.log;
import gfm.core.text;
import gfm.opengl;
import gfm.opengl.uniform;

void getUniformBlocks(OpenGL gl_, GLProgram program_)
{
	GLint numActiveBlocks;
	glGetProgramiv(program_.handle, GL_ACTIVE_UNIFORM_BLOCKS, &numActiveBlocks);
	gl_.runtimeCheck();
	
	for(GLint i = 0; i < numActiveBlocks; ++i)
		new GLUniformBlock(gl_, program_, i);
}

GLUniformBlock[string] activeUniformBlocks;

static bool isaBlockUniform(GLProgram program_, string name_)
{
	foreach(GLUniformBlock block; activeUniformBlocks)
	{
		if(block.isUsedByProgram(program_) && block.hasUniform(name_))
			return true;
	}

	return false;
}

static GLUniformBlock getUniformBlock(string name_)
{
	if(name_ in activeUniformBlocks)
		return activeUniformBlocks[name_];
	else
		return null;
}

class GLUniformBlock
{
	string name;

	private
	{
		OpenGL _gl;

		GLuint[] usedByProgram;

		GLBlockUniform[string] _activeUniforms;

		GLsizei _uniformsNum;
		GLsizei[] _uniformsIndices;

		GLsizei _dataSize;
		GLuint _dataBuffer; // TODO G replace with GLBuffer and use its .setSubData
		GLuint _bindingPoint;
	}

	override bool opEquals(Object o)
	{
		if (this is o) return true;
		if (this is null || o is null) return false;
		if (typeid(this) != typeid(o)) return false;

		auto other = cast(GLUniformBlock)o;

		return this.name == other.name &&					// block name
			this._uniformsNum == other._uniformsNum &&		// number of uniforms in block
			this._dataSize == other._dataSize &&			// size of uniform data in block
			this._activeUniforms == other._activeUniforms;  // uniforms in block
	}

	this(OpenGL gl_, GLProgram program_, int blockIndex_)
	{
		_gl = gl_;

		_getBlockInfo(program_, blockIndex_);
		_getUniformsInfo(program_);

		// if same-named block already exists
		if(name in activeUniformBlocks)
		{
			auto uniBlock = activeUniformBlocks[name];

			// if blocks have the same definition
			if(uniBlock == this)
			{
				// bind block to buffer
				uniBlock._bindBlock(program_, blockIndex_);
				// memory
				uniBlock.usedByProgram ~= program_.handle;
			}
			// blocks mismatch
			else
			{
				// this is actually allowed in gl, because blocks only have to be consistent WITHIN a shader program
				// disallowed so we can use it consistently ACROSS multiple shader programs
				throw new OpenGLException(format("GLUniformBlock [%s] has multiple conflicting definitions.", name));
			}
		}
		// block name does not yet exist
		else
		{
			// create buffer
			_initBuffer(program_, blockIndex_);

			// bind block to buffer
			_bindBlock(program_, blockIndex_);

			// memory
			usedByProgram ~= program_.handle;
			activeUniformBlocks[name] = this;
		}
	}

	private void _getBlockInfo(GLProgram program_, int blockIndex_)
	{
		// max name length
		GLint blockNameMaxLength;
		glGetProgramiv(program_.handle, GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH, &blockNameMaxLength);
		GLchar[] buffer = new GLchar[blockNameMaxLength + 16];

		_gl.runtimeCheck();

		// block name
		GLsizei dummy;
		glGetActiveUniformBlockName(program_.handle,
									cast(GLuint)blockIndex_,
									cast(GLint)(buffer.length),
									&dummy,
									buffer.ptr);

		name = sanitizeUTF8(buffer.ptr);
		_gl._log.info("block name: " ~ name);
		_gl.runtimeCheck();

		_getBlockInfo(program_, blockIndex_, GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS, &_uniformsNum);
		_gl._log.info("block uniforms num: " ~ to!string(_uniformsNum));

		_uniformsIndices = new GLsizei[_uniformsNum];
		_getBlockInfo(program_, blockIndex_, GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES, _uniformsIndices.ptr);
		_gl._log.info("block uniforms ind: " ~ to!string(_uniformsIndices));

		_getBlockInfo(program_, blockIndex_, GL_UNIFORM_BLOCK_DATA_SIZE, &_dataSize);
		_gl._log.info("block uniforms size: " ~ to!string(_dataSize));
	}

	private void _getBlockInfo(GLProgram program_, int blockIndex_, GLenum param_, GLsizei* dataPtr_)
	{
			glGetActiveUniformBlockiv(  program_.handle,
										cast(GLuint)blockIndex_,
										param_,
										dataPtr_);
			_gl.runtimeCheck();
	}

	private void _getUniformsInfo(GLProgram program_)
	{
		auto datas = new GLBlockUniformsData(_gl, program_, _uniformsIndices);

		for(int i = 0; i < _uniformsNum; i++)
		{
			auto uni = new GLBlockUniform(_gl, program_, datas, i, _uniformsIndices);
			_activeUniforms[uni.name] = uni;
		}
	}

	private void _initBuffer(GLProgram program_, int blockIndex_)
	{
		_createBuffer(program_, blockIndex_);

		_createBindingPoint();

		_bindBuffer();
	}

	private void _createBuffer(GLProgram program_, int blockIndex_)
	{
		// create buffer
		glGenBuffers(1, &_dataBuffer);

		// init buffer data
		glBindBuffer(GL_UNIFORM_BUFFER, _dataBuffer);
		glBufferData(GL_UNIFORM_BUFFER, _dataSize, null, GL_DYNAMIC_DRAW);
		glBindBuffer(GL_UNIFORM_BUFFER, 0);

		_gl.runtimeCheck();
	}

	static private int _bindingPointGenerator;
	private void _createBindingPoint()
	{
		_bindingPoint = _bindingPointGenerator;
		_bindingPointGenerator++;
	}

	private void _bindBuffer()
	{
		// bind buffer to binding point
		glBindBufferRange(GL_UNIFORM_BUFFER, _bindingPoint, _dataBuffer, 0, _dataSize);

		_gl.runtimeCheck();
	}

	private void _bindBlock(GLProgram program_, int blockIndex_)
	{
		// bind block to binding point
		glUniformBlockBinding(program_.handle, blockIndex_, _bindingPoint);

		_gl.runtimeCheck();
	}

	void setUniformData(T)(string uniformName_, T data_)
	{
		if(uniformName_ in _activeUniforms)
		{
			_activeUniforms[uniformName_].setUniformData(_dataBuffer, data_);
			_gl.runtimeCheck();
		}
		else
			throw new OpenGLException(format("Unknown uniform [%s] requested for GLUniformBlock [%s]", uniformName_, name));
	}

	bool isUsedByProgram(GLProgram program_)
	{
		foreach(GLuint prog; usedByProgram)
		{
			if(prog == program_.handle)
				return true;
		}

		return false;
	}

	bool hasUniform(string uniformName)
	{
		foreach(GLBlockUniform uni; _activeUniforms)
		{
			if(uni.name == uniformName)
				return true;
		}

		return false;
	}
}


// temporary storage for data about multiple uniforms in a block
class GLBlockUniformsData
{
	GLsizei[] nameLengths;
	//GLsizei[] blockIndices;
	GLsizei[] offsets;
	GLsizei[] arrayStrides;
	GLsizei[] matrixStrides;
	GLsizei[] rowMajors;

	this(OpenGL gl_, GLProgram program_, GLsizei[] uniformsIndices_)
	{
		nameLengths = new GLsizei[uniformsIndices_.length];
		//blockIndices = new GLsizei[uniformsIndices_.length];
		offsets = new GLsizei[uniformsIndices_.length];
		arrayStrides = new GLsizei[uniformsIndices_.length];
		matrixStrides = new GLsizei[uniformsIndices_.length];
		rowMajors = new GLsizei[uniformsIndices_.length];
		
		_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_NAME_LENGTH, nameLengths.ptr);		
		//_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_BLOCK_INDEX, blockIndices.ptr);		
		_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_OFFSET, offsets.ptr);		
		_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_ARRAY_STRIDE, arrayStrides.ptr);
		_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_MATRIX_STRIDE, matrixStrides.ptr);
		_getUniformsInfo(gl_, program_, uniformsIndices_, GL_UNIFORM_IS_ROW_MAJOR, rowMajors.ptr);

		debug
		{
			foreach(int i, GLsizei index; uniformsIndices_)
			{
				gl_._log.info("-----");
				gl_._log.info("namelength " ~ to!string(nameLengths[i]));
				//gl_._log.info(to!string(blockIndices[i]));
				gl_._log.info("offset " ~ to!string(offsets[i]));
				gl_._log.info("stride array " ~ to!string(arrayStrides[i]));
				gl_._log.info("stride matrix " ~ to!string(matrixStrides[i]));
				gl_._log.info("rowmajor " ~ to!string(rowMajors[i])); // 1=rowmajor, 0=columnmajor
			}
		}
	}

	private void _getUniformsInfo(OpenGL gl_, GLProgram program_, int[] uniformIndices_, GLenum param_, GLsizei* dataPtr_)
	{		   
		glGetActiveUniformsiv(   program_.handle,
								cast(GLint)uniformIndices_.length,
								cast(GLuint*)uniformIndices_,
								param_,
								dataPtr_);
		gl_.runtimeCheck();
	}
}

// stores information about uniforms belonging to a uniform block
class GLBlockUniform
{
	string name;

	private
	{
		GLenum _type;
		GLint _size;
		size_t _cacheSizeElem;
		size_t _cacheSizeTotal;

		//GLsizei _nameLength;
		//GLsizei _blockIndex;
		GLsizei _offset;
		GLsizei _arrayStride;
		GLsizei _matrixStride;
		GLsizei _rowMajor;

		ubyte[] _value;
	}

	override bool opEquals(Object o)
	{
		if (this is o) return true;
		if (this is null || o is null) return false;
		if (typeid(this) != typeid(o)) return false;

		auto other = cast(GLBlockUniform)o;

		return this.name == other.name &&
			this._type == other._type &&
			this._size == other._size &&
			this._cacheSizeElem == other._cacheSizeElem &&
			this._cacheSizeTotal == other._cacheSizeTotal &&
			//this._nameLength == other._nameLength &&
			//this._blockIndex == other._blockIndex &&
			this._offset == other._offset &&
			this._arrayStride == other._arrayStride &&
			this._matrixStride == other._matrixStride &&
			this._rowMajor == other._rowMajor;
	}

	this(OpenGL gl_, GLProgram program_, GLBlockUniformsData datas_, GLsizei index_, GLsizei[] uniformIndices_)
	{
		auto nameLength = datas_.nameLengths[index_];
		//_blockIndex = datas_.blockIndices[index_];
		_offset = datas_.offsets[index_];
		_arrayStride = datas_.arrayStrides[index_];
		_matrixStride = datas_.matrixStrides[index_];
		_rowMajor = datas_.rowMajors[index_];

		auto index = uniformIndices_[index_]; // index in shader, not in datas arrays

		_getUniformInfo(gl_, program_, index, nameLength);

		debug
		{
			gl_._log.info("---");
			gl_._log.info("name " ~ name);
			gl_._log.info("type " ~ to!string(_type));
			gl_._log.info("size " ~ to!string(_size));
			gl_._log.info("cachesizeelem " ~ to!string(_cacheSizeElem));
			gl_._log.info("cachesizetotal " ~ to!string(_cacheSizeTotal));
		}
	}

	// gets info about a specific uniform specified by the index
	private void _getUniformInfo(OpenGL gl_, GLProgram program_, GLsizei index_, GLsizei nameLength_)
	{
		GLchar[] buffer = new GLchar[nameLength_ + 16];

		GLsizei dummy;
		glGetActiveUniform(program_.handle,
						   cast(GLuint)index_,
						   cast(GLint)(buffer.length),
						   &dummy,
						   &_size,
						   &_type,
						   buffer.ptr);
		gl_.runtimeCheck();

		_cacheSizeElem = GLUniform.sizeOfUniformType(_type);
		_cacheSizeTotal = _cacheSizeElem * _size;

		_value = new ubyte[_cacheSizeTotal];

		name = sanitizeUTF8(buffer.ptr);
	}

	// http://www.opengl-redbook.com/appendices/AppL.pdf
	// http://www.opengl.org/registry/specs/ARB/uniform_buffer_object.txt

	// Sampler types are not allowed inside of uniform blocks.

	// array stride:
	// concerns arrays of non-basic types
	// "The difference in
	// offsets between each pair of elements in the array in basic machine
	// units is referred to as the array stride, and is constant across the
	// entire array.""

	// basic types: no padding needed
	// vectors of basic types: read from 0..N with padding after if needed, padding is never read so padding is not needed
	// arrays of scalars, vectors, matrices: padding of 'arraystride' between all elements
	//			ie. elem0 is at offset, elem1 is at offset + arraystride*1, etc.
	// matrix
	//	column-major (gl):	C columns, R rows
	//						treated as array of C float vectors of each R elements
	//						columnvector1 is at offset, columnvector2 is at offset + matrixstride*1, etc.
	//	row-major (gfm):	C columns, R rows
	//						treated as array of R float vectors of each C elements
	//						rowvector1 is at offset, rowvector2 is at offset + matrixstride*1, etc.
	//
	// array stride is the the byte distance from the beginning of one element to the beginning of the next.
	// matrix stride is the the byte distance from the beginning of one vector to the beginning of the next.

	void setUniformData(T)(GLuint dataBuffer_, T data_)
	{
		scope(exit)
			glBindBuffer(GL_UNIFORM_BUFFER, 0);

		glBindBuffer(GL_UNIFORM_BUFFER, dataBuffer_);

		_checkType!(T)(data_);

		if(!typeIsBasic(_type) && !typeIsMatrix(_type) && !typeIsVector(_type)) // needed?
			throw new OpenGLException(format("Uniformblock var [%s] unsupported type", name));

		_setData!(T)(data_, _offset);
	}

	private void _checkType(T)(T data_) if (!isArray!T)
	{
		if(!GLUniform.typeIsCompliant!(T)(_type))
			throw new OpenGLException(format("Uniformblock var [%s] type mismatch", name));
	}

	private void _checkType(T)(T data_) if (isArray!T)
	{
		if(!GLUniform.typeIsCompliant!(typeof(data_[0]))(_type))
			throw new OpenGLException(format("Uniformblock var [%s] array type mismatch", name));
	}

	// Set basic type
	private void _setData(T) (T data_, GLsizei offset_) if (!isArray!T && !is(typeof(T._isVector)) && !is(typeof(T._isMatrix)))
	{
		glBufferSubData(GL_UNIFORM_BUFFER, offset_, _cacheSizeElem, &data_);
	}

	// Set array
	private void _setData(T)(T data_, GLsizei offset_) if (isArray!T)
	{
		// if basic type without stride, set whole array at once
		if(_arrayStride <= 0 && typeIsBasic(_type))
			glBufferSubData(GL_UNIFORM_BUFFER, offset_, _cacheSizeTotal, cast(void*)data_);
		// has stride or does not contain basic types: handle elements separately
		else
		{
			foreach(i, dat; data_)
				_setData(data_[i], offset_ + _arrayStride * i);
		}
	}

	// Set gfm Vector
	private void _setData(T)(T data_, GLsizei offset_) if(!isArray!T && is(typeof(T._isVector)))
	{
		_setData!(T)(data_, offset_, _cacheSizeElem);
	}

	private void _setData(T)(T data_, GLsizei offset_, GLsizei cacheSizeElem_) if(!isArray!T && is(typeof(T._isVector)))
	{
		glBufferSubData(GL_UNIFORM_BUFFER, offset_, cacheSizeElem_, cast(void*)data_.v);
	}

	// Set gfm Matrix
	private void _setData(T)(T data_, GLsizei offset_) if(!isArray!T && is(typeof(T._isMatrix)))
	{
		if(_rowMajor)
		{
			auto numRows = data_.rows.length;
			
			for(int i = 0; i < numRows; i++)
			{
				auto offset = offset_ + _matrixStride * i;
				auto cacheSizeElem = _cacheSizeElem / numRows;
				_setData!(data_.row_t)(data_.row(i), offset, cast(GLsizei) cacheSizeElem);
			}
		}
		else
		{
			auto numColumns = data_.v.length / data_.rows.length;
			
			for(int i = 0; i < numColumns; i++)
			{
				auto offset = offset_ + _matrixStride * i;
				auto cacheSizeElem = _cacheSizeElem / numColumns; // gl sees matrix as being 1 element, we see it as multiple vectors
				_setData!(data_.column_t)(data_.column(i), offset, cast(GLsizei) cacheSizeElem);
			}
		}
	}

	static bool typeIsBasic(GLenum type)
	{
		switch (type)
		{
			case GL_FLOAT:
			case GL_DOUBLE:
			case GL_INT:
			case GL_UNSIGNED_INT:
			case GL_BOOL:
				return true;

			default:
				return false;
		}
	}

	static bool typeIsVector(GLenum type)
	{
		switch (type)
		{
			case GL_FLOAT_VEC2:
			case GL_FLOAT_VEC3:
			case GL_FLOAT_VEC4:
			case GL_DOUBLE_VEC2:
			case GL_DOUBLE_VEC3:
			case GL_DOUBLE_VEC4:
			case GL_INT_VEC2:
			case GL_INT_VEC3:
			case GL_INT_VEC4:
			case GL_UNSIGNED_INT_VEC2:
			case GL_UNSIGNED_INT_VEC3:
			case GL_UNSIGNED_INT_VEC4:
			case GL_BOOL_VEC2:
			case GL_BOOL_VEC3:
			case GL_BOOL_VEC4:
				return true;

			default:
				return false;
		}
	}

	static bool typeIsMatrix(GLenum type)
	{
		switch (type)
		{
			case GL_FLOAT_MAT2:
			case GL_FLOAT_MAT3:
			case GL_FLOAT_MAT4:
			case GL_FLOAT_MAT2x3:
			case GL_FLOAT_MAT2x4:
			case GL_FLOAT_MAT3x2:
			case GL_FLOAT_MAT3x4:
			case GL_FLOAT_MAT4x2:
			case GL_FLOAT_MAT4x3:
			case GL_DOUBLE_MAT2:
			case GL_DOUBLE_MAT3:
			case GL_DOUBLE_MAT4:
			case GL_DOUBLE_MAT2x3:
			case GL_DOUBLE_MAT2x4:
			case GL_DOUBLE_MAT3x2:
			case GL_DOUBLE_MAT3x4:
			case GL_DOUBLE_MAT4x2:
			case GL_DOUBLE_MAT4x3:
				return true;

			default:
				return false;
		}
	}
}
