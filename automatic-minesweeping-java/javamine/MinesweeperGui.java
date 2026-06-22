package cs305.minesweeper.javamine;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.UIManager;

import cs305.minesweeper.MinesweeperTile;
import cs305.minesweeper.MinesweeperTile.TileState;

/**
 * Front end for the Minesweeper game board
 * @author Walter and Larry
 *
 */
@SuppressWarnings("serial")//do not need, suppress it
public class MinesweeperGui  extends JFrame{
	
	private JPanel centerPanel;
	
	private GameBoard gameBoard; 
	private int rows;
	private int columns;
	private int numberOfTiles;

	private MinesweeperButton[] tileButtons;
	private JLabel minesLeft;

	/**
	 * creates a GUI for the inputed game board
	 * @param mb
	 */
	public MinesweeperGui(GameBoard mb){

		if (mb != null){
			gameBoard = mb;
			Dimension d = mb.getBoardSize();
			rows = d.width;
			columns = d.height;
			numberOfTiles = rows*columns;
			tileButtons = new MinesweeperButton[numberOfTiles];
		}else{
			System.err.println("The Gui must be passed a non-null GameBoard!");
			return;
		}
		createGui();

		addPanelsAndRender();

	}
	
	/**
	 * sets up on how the GUI looks
	 */
	private void createGui() {
		changeLookandFeel();

		//sets the menu bar
		setJMenuBar(createMenuBar());
		setLayout(new BorderLayout(5, 5));

		centerPanel = new JPanel();

		GridLayout gl = new GridLayout(rows, columns, 0, 0);
		centerPanel.setLayout(gl);
		
		minesLeft = new JLabel("Mines: "+ gameBoard.getBombsLeft());
		add(minesLeft, BorderLayout.SOUTH);
		
		createAndInitializeButtons();
		
	}

	/**
	 * creates a button for each tile in the game board
	 */
	private void createAndInitializeButtons() {
		for(int i = 0; i< numberOfTiles;i++){
			int y= i/columns;
			int x = i%rows ;
			
			MinesweeperButton tile = new MinesweeperButton(this.gameBoard.getTile(x, y) ,"   ");
		
			//defaults for button
			tile.setFocusable(false);
			tile.setBackground(Color.LIGHT_GRAY);

			centerPanel.add(tile);
			tileButtons[i] = tile;
		}
	}

	/**
	 * sets the panels, sets frame defaults
	 */
	private void addPanelsAndRender() {
		
		add(centerPanel, BorderLayout.CENTER);
		
		setTitle("Minesweeper");
		setLocationRelativeTo(null);//top left corner of frame is placed in center of screen
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		pack();//condense the frame
		setResizable(false);//game can not change its size
		
	}

	/**
	 * changes the look and feel of the gui according to operating system
	 */
	private void changeLookandFeel(){

		String os = System.getProperty("os.name").toLowerCase();
		boolean osx = os.startsWith("mac os x");
		//changes look and feel according to operating system, extra feature makes it look pretty  :)
		if(osx){
			try {	//if mac make it look like a mac
				String macString = "javax.swing.plaf.mac.MacLookAndFeel";
				UIManager.setLookAndFeel(macString);
			} catch (Exception e){}
		}else{
			try {//else try to use nimbus
				String nimbus = "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel";
				UIManager.setLookAndFeel(nimbus);
			} catch (Exception e) {}
		}//all else fails, uses default
	}

	/**
	 * creates the menu bar and game options that are used in the game
	 * @return
	 */
	private JMenuBar createMenuBar(){

		JMenuBar menuBar = new JMenuBar();

		//Game Menu
		JMenu gameMenu = new JMenu("Game");
		gameMenu.getAccessibleContext().setAccessibleDescription("Game options");

		//creates Menu Items
		JMenuItem newGameItem = new JMenuItem("New Game", KeyEvent.VK_N);
		JMenuItem exitItem = new JMenuItem("Exit", KeyEvent.VK_X);

		//create new game by reseting the board, saves memory
		newGameItem.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				setVisible(false);
				gameBoard.resetBoard();
				setVisible(true);
			}
		});


		//exits program on exit
		exitItem.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				System.exit(0);
			}
		});

		gameMenu.add(newGameItem);
		gameMenu.add(exitItem);

		menuBar.add(gameMenu);

		return menuBar;
	}
	
	/**
	 * updates the GUI from the inputed tiles
	 * @param tiles
	 */
	public void updateBoard(MinesweeperTile[] tiles){

		for(int i = 0; i < tiles.length;i++){
			if(tiles[i].getState() == TileState.Exposed){
				tileButtons[i].updateTile(tiles[i]);
				tileButtons[i].setEnabled(false);
			} else if(tiles[i].getState() == TileState.Hidden){
				tileButtons[i].setBackground(Color.LIGHT_GRAY);
			} else if (tiles[i].getState() == TileState.Flagged){
				tileButtons[i].setBackground(Color.RED);
			}
		}
	}
	
	/**
	 * opposite effect of updateBoard, hides exposed tiles
	 * used to reset the board
	 * @param tiles
	 */
	public void hideBoard(MinesweeperTile[] tiles){
		for(int i = 0; i < tiles.length;i++){
			
			if(tiles[i].getState() == TileState.Exposed){
				tileButtons[i].setText("   ");
				tileButtons[i].setEnabled(true);
			}else if(tiles[i].getState() == TileState.Flagged){
				tileButtons[i].setBackground(Color.LIGHT_GRAY);//incase it is a flag
			}
			
		}
		
	}
	
	/**
	 * updates a single tile
	 * @param tile
	 * @param i
	 */
	public void updateTile(MinesweeperTile tile, int i){
		tileButtons[i].updateTile(tile);
	}

	/**
	 * sets the mines left label to inputed amount
	 * used for flag clicking and reseting the board
	 * @param mines
	 */
	public void setMinesLeftLabel(int mines){
		if(mines <= gameBoard.getBombsLeft())
			minesLeft.setText("Mines: " + mines);
	}
	
	/**
	 * displays the GUI if it is visible
	 */
	public void displayGui(){
		if (!isVisible())
			setVisible(true);
	}
}
