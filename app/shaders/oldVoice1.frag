#version 330 core

uniform vec3 uCamera;
uniform vec4 uPoint1;
uniform vec4 uPoint2;


in     vec3 vPosition;
in     vec3 vNormal;
in     vec3 vTang;
in     vec3 vBino;
in     vec2 vUV;
in     vec3 vEye;
in     vec3 vPos;
in 	   mat3 vINormMat;
in     float vLength;

out     vec4 fragColor;

vec3 hsv(float h, float s, float v){
  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

#define STEPS 10
float stepDepth = .1;
vec4 volumeColor( vec3 ro , vec3 rd , mat3 iBasis ){

  vec3 col = vec3( 0. );
  float lum = 0.;
  for( int i = 0; i < STEPS; i++ ){

    vec3 p = ro + rd * float( i ) * stepDepth;
    
    p = iBasis * p;
    lum += abs(sin( p.x * 20. ) * sin( p.z * 40.3 )) /5.;
    col += hsv( lum / 1., 1. , 1. ) * lum;

  } 

  return vec4( col , lum ) / float( STEPS );


}

void main() {

	vec3 col = vec3( vUV.x , vUV.y , 1. );

	vec4 volCol = volumeColor( vPos , vEye , vINormMat );
  
    fragColor = vec4( volCol.xyz , volCol.w );


}