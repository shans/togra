from OpenGL.GL.shaders import *
from OpenGL.GL import *
from vao import VAO
import types

class Shader:
  def __init__(this):
    VERTEX_SHADER = compileShader("""
      #version 120

      #define MAX_LIGHTS 3

      attribute vec3 VertexPosition;
      attribute vec3 VertexNormal;

      struct LightStruct {
	vec3 location;
	vec4 diffuse;
	vec4 ambient;
	vec4 specular;
      };

      uniform LightStruct Lights[MAX_LIGHTS];

      varying vec3 baseNormal;
      varying vec3 ECLightHalf[MAX_LIGHTS];
      varying vec3 ECLightLoc[MAX_LIGHTS];

      void main() {
	gl_Position = gl_ModelViewProjectionMatrix * vec4(VertexPosition, 1.0);
        baseNormal = gl_NormalMatrix * VertexNormal;

	int i;
	for (i = 0; i < MAX_LIGHTS; i++) {
	  ECLightLoc[i] = normalize(Lights[i].location);
	  ECLightHalf[i] = normalize(ECLightLoc[i] - vec3(0, 0, -1));
	}
      }""", GL_VERTEX_SHADER)

    FRAGMENT_SHADER = compileShader("""
      #version 120

      #define MAX_LIGHTS 3

      struct MaterialStruct {
	vec4 ambient;
	vec4 diffuse;
	vec4 specular;
	float shininess;
      }; 

      uniform MaterialStruct Material;
      uniform vec4 GlobalAmbient;

      struct LightStruct {
	vec3 location;
	vec4 diffuse;
	vec4 ambient;
	vec4 specular;
      };

      uniform LightStruct Lights[MAX_LIGHTS];

      varying vec3 baseNormal;
      varying vec3 ECLightHalf[MAX_LIGHTS];
      varying vec3 ECLightLoc[MAX_LIGHTS];
      
      void main() {
	vec4 fragColor = GlobalAmbient * Material.ambient;
	int i;
	for (i = 0; i < MAX_LIGHTS; i++) {
	  float diffuse = dot(baseNormal, ECLightLoc[i]);
	  float diffuseWeight = max(0.0, diffuse);
     
	  float specWeight = 0.0;
	  if (diffuse > -0.05) {
	    specWeight = pow(max(0.0, dot(ECLightHalf[i], baseNormal)), 
			     Material.shininess);
	  }

	  fragColor += (
	    (Lights[i].ambient * Material.ambient) +
	    (Lights[i].diffuse * Material.diffuse * diffuseWeight) + 
	    (Lights[i].specular * Material.specular * specWeight));
	}
	gl_FragColor = clamp(fragColor, 0.0, 1.0);
      }""", GL_FRAGMENT_SHADER)

    this.PROGRAM = compileProgram(VERTEX_SHADER, FRAGMENT_SHADER)

    this.vao = VAO()

  def use(this):
    glUseProgram(this.PROGRAM)
  def bind(this):
    this.vao.bind()
  def unbind(this):
    this.vao.unbind()
  def trySetVarying(this, name, value, size, stride):
    pos = glGetAttribLocation(this.PROGRAM, name)
    if (pos == -1):
      return False
    this.vao.associateVertexAttribVBO(pos, value, size, stride)
    return True
  def trySetUniform(this, name, value):
    print name
    pos = glGetUniformLocation(this.PROGRAM, name)
    if pos == -1:
      return False
    if type(value) == types.FloatType:
      glUniform1f(pos, value)
    elif len(value) == 3:
      glUniform3f(pos, value[0], value[1], value[2])
    elif len(value) == 4:
      glUniform4f(pos, value[0], value[1], value[2], value[3])
    else:
      return False
    return True
  def trySetUniformStruct(this, name, item, value):
    return this.trySetUniform(name + "." + item, value)
  def trySetUniformArrStruct(this, name, pos, item, value):
    return this.trySetUniform("%s[%d].%s" % (name, pos, item), value)
  def setF(this, name, *args):
      if len(args) == 1 and this.trySetUniform(name, *args):
	return
      if len(args) == 3 and this.trySetVarying(name, *args):
	return
      if len(args) == 2 and this.trySetUniformStruct(name, *args):
	return
      if len(args) == 3 and this.trySetUniformArrStruct(name, *args):
	return
      raise AttributeError, name
  def __getattr__(this, name):
    if (name.startswith("set")):
      return lambda *args: this.setF(name[3:], *args)
