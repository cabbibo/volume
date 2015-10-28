#version 410 


uniform mat4 uModel;
uniform vec3 uCamera;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition;
uniform float uRepelStrength;
uniform float uTime;

//uniform vec4 uPoints[10];
uniform vec4 uPoint1;
uniform vec4 uPoint2;
uniform vec4 uPoint3;
uniform vec4 uPoint4;
uniform vec4 uPoint5;
uniform vec4 uPoint6;
uniform vec4 uPoint7;
uniform vec4 uPoint8;




in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec3 vTang;
out     vec3 vBino;
out     vec2 vUV;
out     vec3 vEye;
out     vec3 vPos;
out     float vLength;
out     mat3 vINormMat;


#define NUM_POINTS 8


mat3 matInverse( mat3 m ){
    
  
    vec3 a = vec3(
      
        m[1][1] * m[2][2] - m[2][1] * m[1][2],
        m[0][2] * m[2][1] - m[2][2] * m[0][1],
        m[0][1] * m[1][2] - m[1][1] * m[0][2]
        
    );
    
    vec3 b = vec3(
      
        m[1][2] * m[2][0] - m[2][2] * m[1][0],
        m[0][0] * m[2][2] - m[2][0] * m[0][2],
        m[0][2] * m[1][0] - m[1][2] * m[0][0]
        
    );
    
     vec3 c = vec3(
      
        m[1][0] * m[2][1] - m[2][0] * m[1][1],
        m[0][1] * m[2][0] - m[2][1] * m[0][0],
        m[0][0] * m[1][1] - m[1][0] * m[0][1]
        
    );
    
    
    return mat3( 
        
       a.x , a.y , a.z ,
       b.x , b.y , b.z ,
       c.x , c.y , c.z
        
    );
    
 
  
    
}

vec3 cubicCurve( float t , vec3  c0 , vec3 c1 , vec3 c2 , vec3 c3 ){
  
  float s  = 1. - t; 

  vec3 v1 = c0 * ( s * s * s );
  vec3 v2 = 3. * c1 * ( s * s ) * t;
  vec3 v3 = 3. * c2 * s * ( t * t );
  vec3 v4 = c3 * ( t * t * t );

  vec3 value = v1 + v2 + v3 + v4;

  return value;

}



vec3 cubicFromValue( in float val , in vec3 points[NUM_POINTS] , out vec3 upPos , out vec3 doPos ){


  vec3 p0 = vec3(0.);
  vec3 v0 = vec3(0.);
  vec3 p1 = vec3(0.);
  vec3 v1 = vec3(0.);

  vec3 p2 = vec3(0.);



  float base = val * (float(NUM_POINTS)-1.);
  float baseUp   = floor( base );
  float baseDown = ceil( base );
  float amount = base - baseUp;

  if( baseUp == 0. ){

      p0 = points[ int( baseUp ) ];
      p1 = points[ int( baseDown ) ];
      p2 = points[ int( baseDown + 1. ) ];


      v1 = .5 * ( p2 - p0 );

  }else if( baseDown == float(NUM_POINTS-1) ){

      p0 = points[ int( baseUp )   ];
      p1 = points[ int( baseDown ) ];
      p2 = points[ int( baseUp - 1. ) ];

      v0 = .5 * ( p1 - p2 );

  }else{

      p0 = points[ int( baseUp ) ];
      p1 = points[ int( baseDown ) ];


      vec3 pMinus;

      pMinus = points[ int( baseUp - 1. ) ];
      p2 = points[ int( baseDown + 1. ) ];

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

    float val = aUV.x * .99;



    vLength = length(uPoint1-uPoint4);


    


    vec3 upPos;
    vec3 doPos;



    vec3 points[NUM_POINTS] = vec3[]( uPoint1.xyz , uPoint2.xyz , uPoint3.xyz , uPoint4.xyz , uPoint5.xyz , uPoint6.xyz , uPoint7.xyz , uPoint8.xyz);

    vec3 pos = cubicFromValue( val , points , upPos , doPos );
    

    vec3 d1 = normalize( pos - upPos );
    vec3 d2 = normalize( doPos - pos );

    vec3 curveDir =( d1 + d2 )/ 2.;
    vec3 curveX = normalize( cross( d1 , d2 ) );
    vec3 curveY = normalize( cross( curveDir, curveX ) );


    //float left = isLeft( doPos , pos , upPos );
    
    float dirAngle = acos(dot(d1,d2) / (length(d1)* length(d2)));

  //  if(  left > 0.  ){ curveY *= -1.;  curveX *= -1.; }

    float angle = aPosition.y * 2. * 3.14159;
    float fall = (.5-abs( aUV.x - .5));
    float radius = (abs(sin( uTime * .5  + aUV.x * 20.)) + 1.) *.2 * fall + .3 * pow( fall , .1);
    //float radius =.2;

    vec3 fPos = pos + radius * curveX * sin( angle ) + radius * curveY * cos( angle );

    //fPos = pos + vec3( 0., 1. * position.y , 0.);

    vec3 norm = normalize( fPos - pos );

    vPosition = fPos;
    vNormal = norm;
    vTang   = cross( norm , normalize( curveDir ));
    vBino   = normalize( curveDir );

    vec3 vNorm = vNormal;
    mat3 normMat = mat3(
      vNorm.x , vNorm.y , vNorm.z ,
      vTang.x , vTang.y , vTang.z ,
      vBino.x , vBino.y , vBino.z 
    );

  //normMat = normalMatrix * normMat;
    vINormMat = matInverse( normMat );

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

    vPos = fPos;
    vEye = uCamera; //(uInverseModel * vec4(uCamera,1.)).xyz;
    //vEye = normalize( c - vPos );

    gl_Position = uViewProjection * vec4(vPosition, 1.0);

}