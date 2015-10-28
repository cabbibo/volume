#version 330 core

#define NUM_POINTS 8


uniform vec3 uCamera;


uniform vec4 uPoints[ NUM_POINTS ];


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


const float MAX_TRACE_DISTANCE = 1.5;           // max trace distance
const float INTERSECTION_PRECISION = 0.04;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;


vec3 hsv(float h, float s, float v){
  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}


float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
	
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}

#define STEPS 10
float stepDepth = .04;
vec4 volumeColor( vec3 ro , vec3 rd ){

  vec3 col = vec3( 0. );
  float lum = 0.;
  float s = 0.;
  for( int i = 0; i < STEPS; i++ ){
  	s = float( i );

    vec3 p = ro + rd * s * stepDepth;
    
    p = p;
    lum = noise( p  * 10. );

    lum += noise( p  * 60. );
    lum += noise( p  * 20. );
    lum += noise( p  * 4. );
    lum /= 4.;


    col = hsv(s  / float( STEPS ), 1. , 1. );
    if( lum > .5 ){ break; }

  } 

  return vec4( col , s  / float( STEPS ) );// / float( STEPS );


}


float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}

float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}

vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
    vec2 res =  vec2( 1000000. , -100. );
    vec2 res2;

    
    for( int i = 0; i < ( NUM_POINTS - 1 ); i++ ){
   	  res2 = vec2( sdCapsule( pos , uPoints[i].xyz , uPoints[i+1].xyz , .1 ) , 2. );
   	  res = smoothU( res , res2 , .1 );

    }
/*
   	res2 = vec2( sdCapsule( pos , uPoint2.xyz , uPoint3.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );

   	res2 = vec2( sdCapsule( pos , uPoint3.xyz , uPoint4.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );

   	res2 = vec2( sdCapsule( pos , uPoint4.xyz , uPoint5.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );

   	res2 = vec2( sdCapsule( pos , uPoint5.xyz , uPoint6.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );

   	res2 = vec2( sdCapsule( pos , uPoint6.xyz , uPoint7.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );

   	res2 = vec2( sdCapsule( pos , uPoint7.xyz , uPoint8.xyz , .1 ) , 2. );
   	res = smoothU( res , res2 , .1 );*/


    return res;
    
}

vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    
    float h =  INTERSECTION_PRECISION*2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
      vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
    
}




// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}

void main() {

	vec3 ro = vPosition;
  	vec3 rd = normalize( vPosition - vEye );



	vec3 col = vNormal * .5 + .5;

	vec2 res = calcIntersection( ro , rd );


  	if( res.y > .5 ){

    	vec3 pos = ro + rd * res.x;
    	vec3 norm = calcNormal( pos );

   	 	col = vec3( -dot( norm , rd ) );// * .5 + .5;
   	}

	vec4 volCol = volumeColor( ro , rd );
  
  	volCol.xyz *= col;
    fragColor = vec4( 1. - col  *pow( volCol.w , .3) , 1.);


}