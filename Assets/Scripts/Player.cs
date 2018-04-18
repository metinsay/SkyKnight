using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Player : MonoBehaviour
{
    public float gravity = 200f;
	public float drag = 0.01f;
	public float lift_factor = 200f;
	Rigidbody2D rb;
	Vector2 vel; // (vx,vy,vz)
	Vector2 acc; // (ax,ay,az)
    // Use this for initialization

    /*double GetLift(double v, double x)
    {
        return 0.002 * Math.Pow(v, 2) * Math.Pow(x, 0.3);
    }*/
	// returns updated velocity
	Vector3 ForwardKinematics(Vector3 v, Vector3 a, Vector3 angle)
	{
		double speed = v.magnitude;
		Vector3 normal_dir = new Vector3 (- angle.y, angle.x, 0);
		Vector3 lift_acc = (float)(Math.Pow (speed, 2)) * normal_dir.normalized;
		Vector3 drag_acc = -drag * v.normalized;
		a = a + (lift_acc + drag_acc);
		return v = v + Time.deltaTime*a;
	}
    void Start()
    {
		rb = GetComponent<Rigidbody2D> ();
		vel = new Vector3(0,0,0); // (vx,vy,vz)
		acc = new Vector3(0, -gravity,0); // (ax,ay,az)
    }

    // Update is called once per frame
    void FixedUpdate()
    {
        Vector2 mouse = Camera.main.ScreenToWorldPoint(Input.mousePosition);
        Vector2 pos = transform.position;

        Debug.DrawRay(transform.position, mouse, Color.red);

        transform.eulerAngles = new Vector3(0, 0, 200 - Vector2.SignedAngle(mouse, pos));
        //transform.rotation = Quaternion.LookRotation((mouse - pos).normalized);

		vel = ForwardKinematics (vel, acc, transform.eulerAngles);
		Vector3 pos_change = vel * (float)(Time.deltaTime);
		transform.position += pos_change;
		//GameObject.Find("Main Camera").transform.position = transform.position;
    }
}
