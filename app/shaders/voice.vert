#version 410 


uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition;
uniform float uRepelStrength;

uniform vec4 uPoints[10];



in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;
out     float vLength;



vec3 cubicCurve( float t , vec3  c0 , vec3 c1 , vec3 c2 , vec3 c3 ){
  
  float s  = 1. - t; 

  vec3 v1 = c0 * ( s * s * s );
  vec3 v2 = 3. * c1 * ( s * s ) * t;
  vec3 v3 = 3. * c2 * s * ( t * t );
  vec3 v4 = c3 * ( t * t * t );

  vec3 value = v1 + v2 + v3 + v4;

  return value;

}

vec3 cubicFromValue( in float val , in vec3 point0 , in vec3 point1 , in vec3 point2 , in vec3 point3 , out vec3 upPos , out vec3 doPos ){


  vec3 p0 = vec3(0.);
  vec3 v0 = vec3(0.);
  vec3 p1 = vec3(0.);
  vec3 v1 = vec3(0.);

  vec3 p2 = vec3(0.);


  float base = val * 3.;
  float baseUp   = floor( base );
  float baseDown = ceil( base );
  float amount = base - baseUp;

  if( baseUp == 0. ){

      p0 = point0;
      p1 = point1;
      p2 = point2;


      v1 = .5 * ( p2 - p0 );

  }else if( baseDown == 3. ){

      p0 = point2;
      p1 = point3;
      p2 = point1;

      v0 = .5 * ( p1 - p2 );

  }else if( baseUp == 1. ){

      p0 = point1;
      p1 = point2;


      vec3 pMinus;

      pMinus = point0;
      p2 = point3;

      v1 = .5 * ( p2 - p0 );
      v0 = .5 * ( p1 - pMinus );

  }


  vec3 c0 = p0;
  vec3 c1 = p0 + v0/3.;
  vec3 c2 = p1 - v1/3.;
  vec3 c3 = p1;




  vec3 pos = cubicCurve( amount , c0 , c1 , c2 , c3 );

  upPos = cubicCurve( amount  + .01 , c0 , c1 , c2 , c3 );
  doPos = cubicCurve( amount  - .01 , c0 , c1 , c2 , c3 );

  return pos;


}


// from
// http://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line
float isLeft( vec3 a , vec3 b , vec3 c ){
  return ((b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x));
}


void main() {

    float val = aUV.x * .95;



    vLength = length(uPoints[0]-uPoints[3]);


    


    vec3 upPos;
    vec3 doPos;


    vec3 pos = cubicFromValue( val , uPoints[0].xyz , uPoints[1].xyz , uPoints[2].xyz , uPoints[3].xyz , upPos , doPos );
    

    vec3 d1 = normalize( pos - upPos );
    vec3 d2 = normalize( doPos - pos );

    vec3 curveDir =( d1 + d2 )/ 2.;
    vec3 curveX = normalize( cross( d1 , d2 ) );
    vec3 curveY = normalize( cross( curveDir, curveX ) );


    float left = isLeft( doPos , pos , upPos );
    
    float dirAngle = acos(dot(d1,d2) / (length(d1)* length(d2)));

    if(  left > 0.  ){ curveY *= -1.;  curveX *= -1.; }

    float angle = aPosition.y * 2. * 3.14159;
    float radius = .03;

    vec3 fPos = pos + radius * curveX * sin( angle ) + radius * curveY * cos( angle );

    //fPos = pos + vec3( 0., 1. * position.y , 0.);

    vec3 norm = normalize( fPos - pos );

    vPosition = fPos;
    vNormal = norm;
    vUV = aUV;
    //vEye = normalize( cameraPosition - vPos );



    //vec3 dir = uEndPoint - uStartPoint;

    //vec3 pos = uStartPoint + dir * aPosition.x;
    // Pass some variables to the fragment shader
    //vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    //vPosition = pos;

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    //vNormal   = vec3(uModel * vec4(aNormal, 0.0));
    //vUV = aUV;

    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}