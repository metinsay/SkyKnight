using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Player : MonoBehaviour
{
    public double gravity = 200f;
    public double drag = 0.01f;
    // Use this for initialization

    double GetLift(double v, double x)
    {
        return 0.002 * Math.Pow(v, 2) * Math.Pow(x, 0.3);
    }
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        Vector2 mouse = Camera.main.ScreenToWorldPoint(Input.mousePosition);
        Vector2 pos = transform.position;
        Debug.DrawRay(transform.position, Camera.main.ScreenToWorldPoint(Input.mousePosition), Color.red);

        transform.eulerAngles = new Vector3(0, 0, 200 - Vector2.SignedAngle(mouse, pos));
        //transform.rotation = Quaternion.LookRotation((mouse - pos).normalized);
    }
}
