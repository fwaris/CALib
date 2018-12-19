#load "SetupVideo.fsx"
open OpenCvSharp
open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging

let f1 = @"D:\repodata\calib\dsst_soc_all_gen\IPD_3.600000_segs.mp4"

let drawFrame (ctrl:Control, mat:Mat) =
    use g = ctrl.CreateGraphics()
    use bmp = OpenCvSharp.Extensions.BitmapConverter.ToBitmap(mat)
    let rect = RectangleF(0.f,0.f,float32 ctrl.Width,float32 ctrl.Height)
    g.DrawImage(bmp,0.f,0.f,rect,GraphicsUnit.Pixel)
    printfn "."
    
 
let showOnImage image file = 
    let clipIn = new VideoCapture(f1:string)
    for f in 1 .. clipIn.FrameCount do
        let m = new Mat()
        if clipIn.Read(m) then
            drawFrame(image, m)
        m.Release()
    clipIn.Release()

let getFrame file n = 
    let clipIn = new VideoCapture(f1:string)
    let _ = clipIn.Set(CaptureProperty.PosFrames, float n);
    let mat = new Mat()
    let resp = 
        if clipIn.Read(mat) then
                let ptr = mat.CvPtr
                let step = mat.Step()
                //let bmp = new Bitmap(mat.Cols, mat.Rows, step |> int,PixelFormat.Format24bppRgb,ptr);
                let bmp = OpenCvSharp.Extensions.BitmapConverter.ToBitmap(mat)
                
                Some(bmp)
            else
                None
    mat.Release()
    clipIn.Release()
    resp

let frameCount f = 
    let clipIn = new VideoCapture(f1:string)
    let fc = clipIn.FrameCount
    clipIn.Release()
    fc

let show (f:string) =
    let form = new Form()
    form.Width  <- 500
    form.Height <- 500
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 1
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100.f)) |> ignore
    grid.RowCount <- 2
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,90.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.AutoSize)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    let slider = new TrackBar()
    slider.Maximum <- frameCount f - 1
    slider.Dock <- DockStyle.Fill
    grid.Dock <- DockStyle.Fill
    form.Controls.Add(grid)
    let image = new System.Windows.Forms.PictureBox()
    image.SizeMode <- PictureBoxSizeMode.Zoom
    image.Dock <- DockStyle.Fill
    grid.Controls.Add(image)
    grid.Controls.Add(slider)
    let bt = new Button()
    bt.Text <- "Show"
    bt.Click.Add(fun _ -> showOnImage image f)
        //match getFrame f with
        //| Some bmp -> image.Image <- bmp
        //| None -> ())
    grid.Controls.Add(bt)
    slider.ValueChanged.Add(fun e -> 
        match getFrame f slider.Value with
        | Some m -> image.Image <- m
        | None -> ()
    )
    form.Show()

    //    do! Async.Sleep 1000
    //async {
    //    }


// show f1
(*
let image = show()
showOnImage image f1 
|> Async.Start


*)